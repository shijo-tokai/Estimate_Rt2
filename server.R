shinyServer(function(input, output) {
    
    covid19j <- reactive({
        covid19j <-fread("https://dl.dropboxusercontent.com/s/6mztoeb6xf78g5w/COVID-19.csv", 
                         encoding="UTF-8")
    })
    
    output$pref <- renderUI({
        checkboxGroupInput(inputId = "prefecture", 
                           label = "都道府県選択: ", 
                           inline=T,
                           choices =  c("北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県",
                                        "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
                                        "新潟県","富山県","石川県","福井県","山梨県","長野県","岐阜県","静岡県","愛知県",
                                        "三重県","滋賀県","京都府","大阪府","兵庫県","奈良県","和歌山県",
                                        "鳥取県","島根県","岡山県","広島県","山口県",
                                        "徳島県","香川県","愛媛県","高知県",
                                        "福岡県","佐賀県","長崎県","熊本県","大分県","宮崎県","鹿児島県","沖縄県"))
    })
    
    
    
    output$Plot <- renderPlot({
        
        # covid19j$確定日 <- lubridate::mdy(covid19j$確定日)
        BeginDate <- ymd(20200115)
        EndDate <- Sys.Date()
        pref <- input$prefecture
        data <- data.frame()
       
        for (i in 1:length(pref)){
            a <- as.data.frame(expand.grid(確定日 = seq.Date(from=BeginDate, to=EndDate, by=1))) %>% 
                dplyr::left_join(covid19j() %>% 
                                     dplyr::mutate(確定日 = mdy(確定日)) %>% 
                                     dplyr::filter(居住都道府県 == pref[i]) %>%
                                     dplyr::group_by(確定日) %>%
                                     dplyr::summarise(感染者数=n()), 
                                 by="確定日") %>%
                dplyr::mutate(感染者数 = ifelse(is.na(感染者数),0,感染者数), 
                                  累積感染者数=cumsum(感染者数))
        
        names(a) = c("date","inf","cum")
        
        t_start <- seq(2, length(a$inf)-13)   
        t_end <- t_start + 13
        b<- estimate_R(a$inf,method = "parametric_si",
                       config = make_config(list(mean_si = 4.8, std_si = 2.3,
                                                 t_start = t_start, 
                                                 t_end = t_end)))

        b <- b$R
        
        data <- data %>% 
            dplyr::bind_rows(data.frame(pref = pref[i], 
                                        b))
        }
        data <- data %>%
            dplyr::mutate(date=BeginDate+t_start-2)
        g <- ggplot()+ 
            geom_line(data = data%>% 
                          mutate(date = as.Date(as.character(date))),
                      mapping = aes(x=date,y=Mean.R., color=pref)) + 
            geom_hline(linetype="dashed",yintercept = 1, color="black")+
            ylim(0,4)+
            scale_x_date(date_breaks = "14 days", date_labels = "%m/%d") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            theme_gray (base_family = "HiraKakuPro-W3")
        
        print(g)
        
    })
    
})
