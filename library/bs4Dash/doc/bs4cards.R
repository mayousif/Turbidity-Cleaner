## ---- eval = FALSE------------------------------------------------------------
#  shiny::shinyApp(
#    ui = bs4DashPage(
#      enable_preloader = TRUE,
#      navbar = bs4DashNavbar(),
#      sidebar = bs4DashSidebar(
#        bs4SidebarMenu(
#          bs4SidebarMenuItem(
#            tabName = "Tab1",
#            text = "Tab 1",
#            icon = "card"
#          ),
#          bs4SidebarMenuItem(
#            tabName = "Tab2",
#            text = "Tab 2",
#            icon = "card"
#          )
#        )
#      ),
#      controlbar = bs4DashControlbar(),
#      footer = bs4DashFooter(),
#      title = "test",
#      body = bs4DashBody(
#        bs4TabItems(
#          bs4TabItem(
#            tabName = "Tab1",
#            bs4InfoBox(
#              title = "Messages",
#              value = "You are on Tab 1",
#              icon = "envelope",
#              tabName = "Tab2"
#            )
#          ),
#          bs4TabItem(
#            tabName = "Tab2",
#            bs4InfoBox(
#              title = "Messages",
#              value = "You are on Tab 2",
#              icon = "envelope",
#              tabName = "Tab1"
#            )
#          )
#        )
#      )
#    ),
#    server = function(input, output) {}
#  )

