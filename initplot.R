if (ptSex == "F") {
  ptSex = "Kvinde"
} else {
  ptSex = "Mand"
}

# Initialize plot
pltydsigm <- plot_ly(strline) %>%
  layout(showlegend = TRUE,
         hovermode = 'x',
         title = paste0(ptSex, ", ", ptAge, " år"),
#         title = "CPR-nummer og navn",
         xaxis = list(title="Dato",
                      gridcolor="#888888",
                      range=c(Xticks[1]-.1,
                              Xticks[length(Xticks)]+.1),
                      tickangle=45,tickvals=Xticks,
                      ticktext=as.POSIXct(Xticks*60*60*24,
                                          origin = "1900-01-01",
                                          tz="GMT"),
                      # rangeselector = list(
                      #   buttons = list(
                      #     list(
                      #       count = 3,
                      #       label = "3 mo",
                      #       step = "month",
                      #       stepmode = "backward"),
                      #     list(
                      #       count = 6,
                      #       label = "6 mo",
                      #       step = "month",
                      #       stepmode = "backward"),
                      #     list(
                      #       count = 1,
                      #       label = "1 yr",
                      #       step = "year",
                      #       stepmode = "backward"),
                      #     list(
                      #       count = 1,
                      #       label = "YTD",
                      #       step = "year",
                      #       stepmode = "todate"),
                      #     list(step = "all"))),
                      
                      rangeslider = list(type = "date")
                      
         ),
         yaxis = list(title="SD",
                      gridcolor="#888888",
                      range=c(-10.5,
                              10.5),
                      tickvals=Yticks_lab,
                      ticktext=as.character(Ylabels),
                      zeroline=FALSE),
         plot_bgcolor="#EEEEEE",
         margin=list(b=100,
                     t=100)
         
  ) %>%
  
  add_segments(x=as.numeric(min(names(Data)[2:ncol(Data)]))-.1,
               xend=as.numeric(max(names(Data)[2:ncol(Data)]))+.1,
               y=1.16730299,
               yend=1.16730299,
               hoverinfo="none",
               line=list(color=~"#000000"),
               showlegend=FALSE) %>%
  add_segments(x=as.numeric(min(names(Data)[2:ncol(Data)]))-.1,
               xend=as.numeric(max(names(Data)[2:ncol(Data)]))+.1,
               y=-1.16730299,
               yend=-1.16730299,
               hoverinfo="none",
               line=list(color=~"#000000"),
               showlegend=FALSE)

# Define line types
linetypes <- c("line",
               "dash",
               "dot")
