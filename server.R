server <- function(input, output) {

   df <- reactive({
       simulate.competing.data(
            n = 200,
            K = 28,
            par0 =
                c(shapeR = 0.34, scaleR = 6.64,
                  shapeM = 2, scaleM = 20.5,
                  shapeD = 1.51, scaleD = 12.23,
                  sigma = 0.24,
                  alphaM = input$alphaM,
                  alphaD = input$alphaD,
                  betaR = input$betaR,
                  betaM = input$betaM,
                  betaD = input$betaD)
        ) %>%
        group_by(id) %>%
        mutate(tTerminal = sum(died*losic) - sum(discharged*losic),
        	   treatment = ifelse(trt==1,"Treated", "Placebo")) %>%
        ungroup %>%
        group_by(trt) %>%
        mutate(id = dense_rank(1000 * tTerminal + id)) %>%
        filter(day <= losic)
    })

   output$trialPlot <- renderPlot({
        print({
        ggplot(df()) +
        facet_wrap("treatment", nrow = 1) +
        geom_point(aes(x = day-1, y = (id),
           shape = state, fill = state), size = 4) +
        scale_x_continuous("Day in ICU",
        				   breaks = 4*(0:7),
        				   minor_breaks = NULL) +
        scale_y_continuous("Participant ID", expand = c(0.01, 0.01)) +
        scale_shape_manual("Status",
              values = c(22,22,24,25),
               breaks = c("Delirium", "No Delirium", "Dead", "Discharged"))+
        scale_fill_manual("Status",
              values = c("#d55e00",
                         "#ffffff",
                         "#0072b2",
                         "#009e73"),
               breaks = c("Delirium", "No Delirium", "Dead", "Discharged"))+
        theme_bw() +
        theme(panel.grid.major.y = element_blank(),
              legend.position = c(.85,.45),
              legend.background = element_rect(color = "grey80"))
        },height = 2000,width = 1200)
   })

   output$outcomeTable1 <- renderDataTable({
		df() %>%
   		group_by(id) %>%
   		summarise(del.days = sum(delirium, na.rm = T),
   				  del.any = sum(delirium, na.rm = T)>0,
   				  losic = losic[1],
   				  died = died[1],
   				  discharged = discharged[1],
   				  censored = 1*(died + discharged == 0),
   				  trt = trt[1]) %>%
   		group_by(trt, died, discharged, censored) %>%
   		summarise(losic = sum(losic),
   				  del.days = sum(del.days, na.rm = T),
   				  del.any = sum(del.any, na.rm = T),
   				  n = n()) %>%
   		group_by(trt) %>%
   		summarise(Died = sum(n*died),
   				  Discharged = sum(n*discharged),
   				  `Any Delirium` = sum(del.any),
   				  `Days with Delirium` = sum(del.days)/100,
   				  `Length of Stay` = sum(losic)/100)%>%
   		pivot_longer(cols = Died:`Length of Stay`, names_to = "var", values_to = "val") %>%
   		pivot_wider(names_from = "trt", values_from = "val", id_cols = "var") %>%
   		transmute(Summary = var, Placebo = `0`, Treated =`1`, Summary = var)
   })

   output$outcomeTable2 <- renderDataTable({
		df() %>%
   		group_by(id) %>%
   		summarise(del.days = sum(delirium, na.rm = T),
   				  del.any = sum(delirium, na.rm = T)>0,
   				  losic = losic[1],
   				  died = died[1],
   				  discharged = discharged[1],
   				  censored = 1*(died + discharged == 0),
   				  trt = trt[1]) %>%
   		group_by(trt, died, discharged, censored) %>%
   		summarise(losic = sum(losic),
   				  del.days = sum(del.days, na.rm = T),
   				  del.any = sum(del.any, na.rm = T),
   				  n = n()) %>%
   		group_by(trt) %>%
      	filter(died == 1) %>%
   		summarise(`Any Delirium` = 100*sum(del.any)/n,
   				  `Days with Delirium` = sum(del.days)/n,
   				  `Length of Stay` = sum(died*losic)/n)%>%
   		pivot_longer(cols = `Any Delirium`:`Length of Stay`, names_to = "var", values_to = "val") %>%
   		pivot_wider(names_from = "trt", values_from = "val", id_cols = "var") %>%
   		transmute(Summary = var, Placebo = `0`, Treated =`1`)
   })

   output$outcomeTable3 <- renderDataTable({
		df() %>%
   		group_by(id) %>%
   		summarise(del.days = sum(delirium, na.rm = T),
   				  del.any = sum(delirium, na.rm = T)>0,
   				  losic = losic[1],
   				  died = died[1],
   				  discharged = discharged[1],
   				  censored = 1*(died + discharged == 0),
   				  trt = trt[1]) %>%
   		group_by(trt, died, discharged, censored) %>%
   		summarise(losic = sum(losic),
   				  del.days = sum(del.days, na.rm = T),
   				  del.any = sum(del.any, na.rm = T),
   				  n = n()) %>%
   		group_by(trt) %>%
      	filter(discharged == 1) %>%
   		summarise(`Any Delirium` = 100*sum(del.any)/n,
   				  `Days with Delirium` = sum(del.days)/n,
   				  `Length of Stay` = sum(died*losic)/n)%>%
   		pivot_longer(cols = `Any Delirium`:`Length of Stay`, names_to = "var", values_to = "val") %>%
   		pivot_wider(names_from = "trt", values_from = "val", id_cols = "var") %>%
   		transmute(Summary = var, Placebo = `0`, Treated =`1`)
   })

}


