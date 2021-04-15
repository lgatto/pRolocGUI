## css for styling app

## CSS for header styling
css_hs <- HTML('
         /* ------ logo ------ */
        .skin-blue .main-header .logo {
          background-color: #808080;
        }
        
        /* ------ logo when hovered ------ */
        .skin-blue .main-header .logo:hover {
          background-color: #808080;
        }
        
         /* ------ navbar (rest of the header) ------ */
        .skin-blue .main-header .navbar {
                              background-color: #C0C0C0;
        }
        
        /* ------ main sidebar ------ */
        .skin-blue .main-sidebar {
                              background-color: #FFFFFF;
        }
        
        /* ------ body ------ */
        .content-wrapper, .right-side {
                            background-color: #FFFFFF;
        }
        
        /* ------ top scroll bar ------ */
         .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
                     transform:rotateX(180deg);
        }
        .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
                     transform:rotateX(180deg);
        }
        
        /* ------ tabsetPanel - color of the font of the inactive tabs (lightgrey) ------ */
        .nav-tabs>li>a  {
                            color: #777; 
              }
       /* ------ tabsetPanel - color of the font when hovering over a tab (darkgrey) ------ */
              .nav>li>a:focus, .nav>li>a:hover {
                            color: #1f1f1f;
       }
       /* ------ tabsetPanel - color of the font of the active cliked tabs (darkgrey) ------ */
              .nav>li>a:active, .nav>li>a:focus, .nav>li>a:hover {
                            color: #1f1f1f;
       }
       .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
                            color: #1f1f1f;
       }      
       
       /* ------ move helper ? icon closer to button ------ */
              .shinyhelper-container {right: 15%}
              
       /* ------ move items in controlbar away from margins ------ */   
       .control-sidebar {
    position: right;
    padding-top: 100px;
    padding-left: 10px;
    padding-right: 10px;
    z-index: 1010;
       }

      ')

css_colours <- function(colors){
  template <- "
.checkboxGroupButtons div.btn-group:nth-child(%s) button {
  background: %s !important;
  color: white !important;
  padding: 5px;
  margin-bottom: 8px;
  border-radius: 10px !important;
  border: none
}"
  paste0(
    apply(cbind(seq_along(colors), colors), 1, function(vc){
      sprintf(template, vc[1], vc[2])
    }),
    collapse = "\n"
  )
}