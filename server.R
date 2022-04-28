server <- function(input, output) {

   df <- reactive({
       simulate.competing.data(
            n = 500,
            K = 28,
            par0 =
                c(shapeR = input$shapeR,
                  scaleR = input$scaleR,
                  shapeM = input$shapeM,
                  scaleM = input$scaleM,
                  shapeD = input$shapeD,
                  scaleD = input$scaleD,
                  sigma = input$sigma,
                  alphaM = input$alphaM,
                  alphaD = input$alphaD,
                  betaR = input$betaR,
                  betaM = input$betaM,
                  betaD = input$betaD)
        )
    })

   output$fullData  <- renderDataTable({
       df() %>%
        pivot_wider(names_from = "day", values_from = "delirium",
                    id_cols = c("id", "losic", "died", "discharged", "trt"))
    })

   output$trialPlot <- renderPlot({
        print({
        df() %>%
        group_by(id) %>%
        mutate(tTerminal = max(died*1/losic) - max(discharged*1/losic),
        	   treatment = ifelse(trt==1,"Treated", "Placebo")) %>%
        ungroup %>%
        group_by(trt) %>%
        filter(id %in% sample(unique(id),40))  %>%
        mutate(id = dense_rank(100000 * tTerminal + id)) %>%
        ungroup %>%
        filter(day <= losic) %>%
        ggplot() +
        facet_wrap("treatment", nrow = 1) +
        geom_point(aes(x = day-1, y = (id),
           shape = state, fill = state), size = 6) +
        scale_x_continuous("Day in ICU",
        				   breaks = 4*(0:7), limits = c(0,29),
        				   minor_breaks = NULL) +
        scale_y_continuous("Participant ID", expand = c(0.05, 0.05)) +
        scale_shape_manual("Status",
              values = c(22,22,24,25),
               breaks = c("Delirium", "No Delirium", "Dead", "Discharged"))+
        scale_fill_manual("Status",
              values = c("#0072b2",
                         "#ffffff",
                         "#d55e00",
                         "#009e73"),
               breaks = c("Delirium", "No Delirium", "Dead", "Discharged"))+
        theme_bw(20) +
        theme(panel.grid.major.y = element_blank(),
              legend.position = c(.95,.15),
              legend.background = element_rect(color = "grey80"))+
        ggtitle("Sample of 20 Patients from Each Treatment Arm")
        },height = 2000,width = 1200)
   })

   #### Summmary Output Tables
   output$outcomeTable1 <- gt::render_gt({
		df() %>%
   		group_by(id) %>%
   		summarise(del.days = sum(delirium, na.rm = T),
   				  del.any = sum(delirium, na.rm = T)>0,
   				  losic = losic[1],
   				  died = died[1],
   				  discharged = discharged[1],
   				  censored = 1*(died + discharged == 0),
   				  trt = trt[1]) %>%
   		group_by(trt) %>%
   		summarise(`Any Delirium (%)` = 100*mean(del.any),
   				  `Days with Delirium (per patient)` = mean(del.days),
   				  `Days with Delirium (per day, %)` =  100*sum(del.days)/sum(losic),
   				  `Length of Stay` = mean(losic),
   				  `Died (%)`= 100*mean(died),
   				  `Discharged (%)` = 100*mean(discharged))%>%
   		pivot_longer(cols = `Any Delirium (%)`:`Discharged (%)`, names_to = "var", values_to = "val") %>%
   		pivot_wider(names_from = "trt", values_from = "val", id_cols = "var") %>%
   		transmute(Summary = var, Placebo = round(`0`,2), Treated =round(`1`,2)) %>%
        gt::gt() %>%
        gt::tab_header("All Patients (n = 500)")
   })

   output$outcomeTable2 <- gt::render_gt({
		df() %>%
        filter(died == 1) %>%
   		group_by(id) %>%
   		summarise(del.days = sum(delirium, na.rm = T),
   				  del.any = sum(delirium, na.rm = T)>0,
   				  losic = losic[1],
   				  died = died[1],
   				  discharged = discharged[1],
   				  censored = 1*(died + discharged == 0),
   				  trt = trt[1]) %>%
   		group_by(trt) %>%
   		summarise(`Any Delirium (%)` = 100*mean(del.any),
   				  `Days with Delirium (per patient)` = mean(del.days),
   				  `Days with Delirium (per day, %)` = 100*sum(del.days)/sum(losic),
   				  `Length of Stay` = mean(losic))%>%
   		pivot_longer(cols = `Any Delirium (%)`:`Length of Stay`, names_to = "var", values_to = "val") %>%
   		pivot_wider(names_from = "trt", values_from = "val", id_cols = "var") %>%
   		transmute(Summary = var,Placebo = round(`0`,2), Treated =round(`1`,2)) %>%
        gt::gt() %>%
        gt::tab_header("Among Patients Who Died")
   })

   output$outcomeTable3 <- gt::render_gt({
		df() %>%
        filter(discharged == 1) %>%
   		group_by(id) %>%
   		summarise(del.days = sum(delirium, na.rm = T),
   				  del.any = sum(delirium, na.rm = T)>0,
   				  losic = losic[1],
   				  died = died[1],
   				  discharged = discharged[1],
   				  censored = 1*(died + discharged == 0),
   				  trt = trt[1]) %>%
   		group_by(trt) %>%
   		summarise(`Any Delirium (%)` = 100*mean(del.any),
   				  `Days with Delirium (per patient)` = mean(del.days),
   				  `Days with Delirium (per day)` = sum(del.days)/sum(losic),
   				  `Length of Stay` = mean(losic)) %>%
   		pivot_longer(cols = `Any Delirium (%)`:`Length of Stay`, names_to = "var", values_to = "val") %>%
   		pivot_wider(names_from = "trt", values_from = "val", id_cols = "var") %>%
   		transmute(Summary = var,Placebo = round(`0`,2), Treated =round(`1`,2)) %>%
        gt::gt() %>%
        gt::tab_header("Among Patients Who Discharged")
   })

   #### Basic Survival Analysis
   survivalData <- reactive({
           group_by(df(), id) %>%
           filter(delirium==1 | day == losic ) %>%
           filter(day == min(day)) %>%
           mutate(delirium = ifelse(is.na(delirium), 0, delirium))
   })

   survivalFit <- reactive({
       survfit(formula = Surv(day, delirium) ~ trt, data = survivalData())
   })


   output$survPlot <- renderPlot({
        print({
            ggsurvplot(fit = survivalFit())
        })
   })
}


