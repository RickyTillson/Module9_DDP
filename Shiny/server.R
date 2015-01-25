library(shiny)
library(markdown)

# stuff
shinyServer(function(input, output) {

    output$sigPlot1 <- renderPlot({
        
        #inputs for 1-sample
        score       <- input$proportion / 100
        base        <- input$base
        target      <- input$target / 100
        tails1      <- input$tails1
        direction1  <- input$direction1
        confidence1 <- input$confidence1 / 100
        if (tails1 == 1) {
            range1       <- qnorm(confidence1)
        } else {
            range1       <- qnorm(1 - (1 - confidence1) / 2)
        }
        se_1 <- sqrt((score * (1 - score)) / base)
        
        # splitting the curve into 3 components
        lcord.x <<- c(score - 4 * se_1,
                      seq(score - 4 * se_1, score - range1 * se_1,length.out = 1000),
                      score - range1 * se_1) 
        lcord.y <<- c(0,
                      dnorm(seq(score - 4 * se_1, score - range1 * se_1, length.out = 1000),score, se_1),
                      0) 
        mcord.x <<- c(score - range1 * se_1,
                      seq(score - range1 * se_1, score + range1 * se_1 , length.out = 1000),
                      score + range1 * se_1)
        mcord.y <<- c(0,
                      dnorm(seq(score - range1 * se_1, score + range1 * se_1, length.out = 1000), score, se_1),
                      0)
        ucord.x <<- c(score + range1 * se_1,
                      seq(score + range1 * se_1, score + 4 * se_1, length.out = 1000),
                      score + 4 * se_1) 
        ucord.y <<- c(0,
                      dnorm(seq(score + range1 * se_1, score + 4 * se_1, length.out = 1000), score, se_1),
                      0) 
        
        x <- seq(score - 4 * se_1, score + 4 * se_1, length.out = 3000)
        y <- dnorm(x, score, se_1)
        
        scale1 <-((score + 4 * se_1) - (score - 4 * se_1)) / 20
        
        l.limit1 <- if (x[1] - scale1 < target - scale1) {
            x[1] - scale1
        } else {
            target - scale1
        }
                        
        u.limit1 <- if (x[3000] + scale1 > target + scale1) {
            x[3000] + scale1
        } else {
            target + scale1
        }
        
        plot(x, y, xlim = c(l.limit1, u.limit1), type = "l", main="", lwd=2, axes = FALSE, xlab = "", ylab = "", col = 'darkgreen')
        abline(v=score, lty=3, lwd = 2, col = 'darkgreen')
        abline(v=target, lty=1, lwd = 2)
        
        if (tails1 ==2) {
            polygon(lcord.x, lcord.y, col='green')
            polygon(ucord.x, ucord.y, col='red')
        } else {
            if (direction1 == 1) {
                polygon(lcord.x, lcord.y, col='green')
            } else {
                polygon(ucord.x, ucord.y, col='red')
            }
        }
    })
    
    output$TestResult1 <- renderUI({
        
        # text advising if significant or not
        str1 <- paste('With a sample size of ', input$base, ', a proportion of ', input$proportion,
              '%', sep = "")
        str2 <- paste('your sample ', if (input$tails1 == 2){
                  if (lcord.x[1000] < input$target / 100 & ucord.x[1] > input$target / 100) {
                      ' IS NOT significantly different from '
                  } else {
                      ' IS significantly different from '
                  }
              } else {
                  if (input$direction1 == 1){
                      if (lcord.x[1000] < input$target / 100) {
                          ' IS NOT significantly above '
                      } else {
                          ' IS significantly above '}
                  } else {
                      if (ucord.x[1] > input$target / 100) {
                          ' IS NOT significantly below '
                      } else {
                          ' IS significantly below '}
                  }
              }
              , 'the target of ', input$target , '% at the ', input$confidence1, '% significance level' , sep="")
        HTML(paste(str1, str2, sep = '<br/>'))
    })
    
    output$sigPlot2 <- renderPlot({
        
        #inputs for 2-sample
        score_1     <- input$proportion1 / 100
        base_1      <- input$base1
        score_2     <- input$proportion2 / 100
        base_2      <- input$base2
        tails2      <- input$tails2
        direction2  <- input$direction2
        confidence2 <- input$confidence2 / 100
        if (tails2 == 1) {
            range2       <- qnorm(confidence2)
        } else {
            range2       <- qnorm(1 - (1 - confidence2) / 2)
        }
        p_2  <- ((base_1 * score_1) + (base_2 * score_2)) / (base_2 + base_1)
        se_2 <- sqrt(p_2 * (1 - p_2) * ((1 / base_1) + (1 / base_2)))
        
        # splitting the curve into 3 components
        lcord.x1 <<- c(score_1 - 4 * se_2,
                       seq(score_1 - 4 * se_2, score_1 - range2 * se_2, length.out = 1000),
                       score_1 - range2 * se_2) 
        lcord.y1 <<- c(0,
                       dnorm(seq(score_1 - 4 * se_2, score_1 - range2 * se_2, length.out = 1000), score_1, se_2),
                       0) 
        mcord.x1 <<- c(score_1 - range2 * se_2,
                       seq(score_1 - range2 * se_2, score_1 + range2 * se_2 , length.out = 1000),
                       score_1 + range2 * se_2)
        mcord.y1 <<- c(0,
                       dnorm(seq(score_1 - range2 * se_2, score_1 + range2 * se_2, length.out = 1000), score_1, se_2),
                       0)
        ucord.x1 <<- c(score_1 + range2 * se_2,
                       seq(score_1 + range2 * se_2, score_1 + 4 * se_2, length.out = 1000),
                       score_1 + 4 * se_2) 
        ucord.y1 <<- c(0,
                       dnorm(seq(score_1 + range2 * se_2, score_1 + 4 * se_2, length.out = 1000), score_1, se_2),
                       0) 
        
        lcord.x2 <<- c(score_2 - 4 * se_2,
                       seq(score_2 - 4 * se_2, score_2 - range2 * se_2, length.out = 1000),
                       score_2 - range2 * se_2) 
        lcord.y2 <<- c(0,
                       dnorm(seq(score_2 - 4 * se_2, score_2 - range2 * se_2, length.out = 1000), score_2, se_2),
                       0) 
        mcord.x2 <<- c(score_2 - range2 * se_2,
                       seq(score_2 - range2 * se_2, score_2 + range2 * se_2 , length.out = 1000),
                       score_2 + range2 * se_2)
        mcord.y2 <<- c(0,
                       dnorm(seq(score_2 - range2 * se_2, score_2 + range2 * se_2, length.out = 1000), score_2, se_2),
                       0)
        ucord.x2 <<- c(score_2 + range2 * se_2,
                       seq(score_2 + range2 * se_2, score_2 + 4 * se_2, length.out = 1000),
                       score_2 + 4 * se_2) 
        ucord.y2 <<- c(0,
                       dnorm(seq(score_2 + range2 * se_2, score_2 + 4 * se_2, length.out = 1000), score_2, se_2),
                       0) 
        
        x_1 <- seq(score_1 - 4 * se_2, score_1 + 4 * se_2, length.out = 3000)
        x_2 <- seq(score_2 - 4 * se_2, score_2 + 4 * se_2, length.out = 3000)
        y_1 <- dnorm(x_1, score_1, se_2)
        y_2 <- dnorm(x_2, score_2, se_2)
        
        scale2 <-((score_1 + 4 * se_2) - (score_1 - 4 * se_2)) / 20
        
        l.limit2 <- if (x_1[1] - scale2 < x_2[1] - scale2) {
            x_1[1] - scale2
        } else {
            x_2[1] - scale2
        }
        
        u.limit2 <- if (x_1[3000] + scale2 > x_2[3000] + scale2) {
            x_1[3000] + scale2
        } else {
            x_2[3000] + scale2
        }        
        
        plot(x_1, y_1, xlim=c(l.limit2, u.limit2), type = "l", axes = F, main="", xlab = "", ylab = "", col = 'blue', lwd = 2)
        abline(v=score_1, lty=3, col = 'blue', lwd = 2)
        lines(x_2, y_2, col = 'red', lwd = 2)
        abline(v=score_2, lty=3, col = 'red', lwd = 2)
        
        if (tails2 ==2) {
            polygon(lcord.x1, lcord.y1, col='green')
            polygon(ucord.x1, ucord.y1, col='red')
            polygon(lcord.x2, lcord.y2, col='green')
            polygon(ucord.x2, ucord.y2, col='red')
        } else {
            if (direction2 == 1) {
                polygon(ucord.x1, ucord.y1, col='red')
                polygon(lcord.x2, lcord.y2, col='green')
            } else {
                polygon(lcord.x1, lcord.y1, col='green')
                polygon(ucord.x2, ucord.y2, col='red')
            }
        }
    }) 
    
    output$TestResult2 <- renderUI({
        
        # text advising if significant or not
        str1 <- paste('With a sample size of ', input$base1, ' and a proportion of ', input$proportion1,
                      '% for sample 1,', sep = "")
        str2 <- paste('and a sample size of ', input$base2, ' and a proportion of ', input$proportion2,
                      '% for sample 2,', sep = "")
        str3 <- paste('sample 2 ', if (input$tails2 == 2){
            if (lcord.x1[1000] < ucord.x2[1] & ucord.x1[1] > lcord.x2[1000]) {
                ' IS NOT significantly different from '
            } else {
                ' IS significantly different from '
            }
        } else {
            if (input$direction2 == 1){
                if (lcord.x2[1000] < ucord.x1[1]) {
                    ' IS NOT significantly above '
                } else {
                    ' IS significantly above '}
            } else {
                if (ucord.x2[1] > lcord.x1[length(lcord.x1)]) {
                    ' IS NOT significantly below '
                } else {
                    ' IS significantly below '}
            }
        }
        , 'sample 1 at the ', input$confidence2, '% significance level' , sep="")
        
       HTML(paste(str1, str2, str3, sep = '<br/>'))
    })
})