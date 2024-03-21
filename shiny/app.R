#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(leaflet.minicharts)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(gridExtra)
library(stringr)
library(shinyjs)
library(plotly)
library(ggrepel)
library(shinydashboard)
library(mapdata)
library(plotrix)
library(scatterpie)
library(leafpop)
library(ggpubr)
library(viridis)
library(highcharter)
#library(rCharts)
library(terra)
library(geojsonio)
library(tigris)
library(dplyr)
library(lattice)
library(ggplot2)
library(forcats)
library(plotly)
library(formattable)
library(hrbrthemes)
#library(devtools)


prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(
  spinner.color = prettyblue,
  spinner.color.background = '#ffffff',
  spinner.size = 3,
  spinner.type = 7
)

colors <-
  c(
    "#232d4b",
    "#2c4f6b",
    "#0e879c",
    "#60999a",
    "#d1e0bf",
    "#d9e12b",
    "#e6ce3a",
    "#e6a01d",
    "#e57200",
    "#fdfdfd"
  )


# user -------------------------------------------------------------
ui <- navbarPage(
  title = "Hampton Roads",
  selected = "overview",
  theme = shinytheme("lumen"),
  tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
  useShinyjs(),
  
  
  
  
  
  
  
  
  
  
  
  
  #Project Overview--------------------------------------------------
  navbarMenu(
    title = "overview",
    tabPanel(
      "Project Introduction",
      value = "overview",
      fluidRow(
        style = "margin: 2px;",
        align = "center",
        # br("", style = "padding-top:2px;"),
        # img(src = "VTDSPG Logo.png", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
        br(""),
        h1(strong("Tracking Indicators of the Economic and Social Mobility of the Black Community in Hampton Roads"),
           br(""),
           h4("Virginia Tech: Data Science for the Public Good Program & CMDA Capstone Team"),
           h4("See 'Meet the Team' tab for detailed team contributions"),
           br()
        )
      ),
      fluidRow(
        style = "margin: 6px;",
        column(
          4,
          h2(strong("The Setting")),
          p(
            a(href = "https://en.wikipedia.org/wiki/Hampton_Roads", "Hampton Roads", target = "_blank"),
            "area consists of ten cities and six counties in the Southeastern region of Virginia. It is ranked as the 33rd largest MSA in the United States, the 8th largest metro area in the Southeast region, and the 2nd largest between Atlanta and Washington, DC."
          ),
          p(
            "The jurisdictions of Hampton Roads are the cities of Chesapeake, Franklin, Hampton, Newport News, Norfolk, Poquoson, Portsmouth, Suffolk, Virginia Beach, and Williamsburg, and the counties of Gloucester, Isle of Wright, James City, Mathews, Southampton, and York."
          ),
          p(
            "The population of the Hampton Roads MSA has been growing over the last decade with an estimated population of approximately 1.7 million in 2020, a 3% increase from 2010. This region accounts for a large percentage – about 20% - of Virginia’s state population.  "
          ),
          p(
            "Each jurisdiction in Hampton Roads has a separate municipal government, unlike some other metro areas. While there are consultations on regional issues, there are more than 20 elected independent municipal governing bodies. As such, it is imperative for our project to examine not only differences with the Virginia population but also within the localities of Hampton Roads."
          ),
          p(
            "Given its location, Hampton Roads has one of the world’s greatest natural harbors, with the ability to accommodate the largest cargo ships on the planet.  It is also known for its large military presence, shipping piers, and miles of waterfront property and beaches. Moreover, due to the diversity of the localities, Hampton Roads have a diverse set of natural resources assets ranging from 26 miles of Atlantic Ocean and Chesapeake Bay beaches, numerous state parks, wildlife refuges, Lake Drummond, and various rivers and waterways."
          )
        ),
        column(
          4,
          h2(strong("Project Background")),
          p(
            "The Black Business Research Analytics Networking and Development (Black BRAND) is a non-profit organization operating in the Hampton Roads region. One goal of Black Brand, which was established in 2016, is to improve the values of the Black families and community within the region."
          ),
          p(
            "Despite the overall economic growth in the United States, there is a significant Black-white wealth gap. In 2019, the median white household held $188,200 in wealth",
            em("– 7.8 times"),
            "that of the typical Black household ($24,100). As such, Black BRAND wants to investigate the economic well-being of the Black community in Hampton Roads.  Moreover, given the recent increase in support for black businesses, it is important to determine whether this has resulted in any economic improvement for the Black community."
          ),
          p(
            "Guided by our meetings with the Black BRAND stakeholders and Claud Anderson's PowerNomics model, there are five main pillars to measure the overall economic and social progress of the Black community in Hampton Roads."
          ),
          img(src = "Flowchart.png", style = "display:inline;margin-left: auto; margin-right: auto;", width =
                "80%"),
        ),
        column(
          4,
          h2(strong("Project Goals")),
          p(
            "Our team aims to create a dashboard that shows the state of the Black community in the Hampton Roads region.  This will enable stakeholders to understand the myriad of past and current factors that affect the economic and social progress of Black households in Hampton. As such, data-driven recommendations can be made to improve the well-being of residents of the region."
          ),
          p(
            "We identified, acquired, and used publicly available data to provide our stakeholders, Black BRAND, with a dashboard that shows a combined view of multiple indicators for the two pillars: Education and Economics."
          ),
          p(
            "For the Education pillar, we utilized indicators across three main areas: "
          ),
          tags$ul(
            tags$li("Educators"),
            tags$li("Educational Attainment"),
            tags$li("Suspension")
          ),
          p(),
          p(
            "Under the Economics pillar, we collected indicators for four distinct issues:"
          ),
          tags$ul(
            tags$li("Income/Wealth"),
            tags$li("Labor Market"),
            tags$li("Homeownership"),
            tags$li("Health"),
            tags$li("Household Wellbeing")
          ),
          p(
            "We conducted a cross-comparison analysis with our indicators across the counties and cities in Hampton Roads.  Our project also compares the Black population against general population in Hampton Roads to determine whether racial differences exist within each locality. Moreover, we also present information for the general population in Virginia."
          ),
          p(
            "The dashboard compiles our findings and allows our stakeholders, and other interested users to explore the information dynamically."
          )
        )
      ),
      fluidRow(align = "center",
               p(tags$small(
                 em('Last updated: May 2022')
               ))
      )
    ),
    
    tabPanel("Hampton Roads Localities",
             fluidRow(
               column(
                 3,
                 p("", style = "padding-top:30px;"),
                 h4(strong("Counties and Cities of Hampton Roads")),
                 tags$ul(
                   tags$li("City of Chesapeake"),
                   tags$li("City of Franklin"),
                   tags$li("City of Hampton"),
                   tags$li("City of Newport News"),
                   tags$li("City of Norfolk"),
                   tags$li("City of Poquoson"),
                   tags$li("City of Portsmouth"),
                   tags$li("City of Suffolk"),
                   tags$li("City of Virginia Beach"),
                   tags$li("City of Williamsburg"),
                   tags$li("Gloucester County"),
                   tags$li("Isle of Wight County"),
                   tags$li("James City County"),
                   tags$li("Mathews County"),
                   tags$li("Southampton County"),
                   tags$li("York County")
                 )
               ),
               column(7,
                      h3(
                        strong("Localities of Hampton Roads"), align = "center"
                      ),
                      withSpinner((
                        plotOutput("hampton_counties_map", width = "100%", height = "600px")
                      ))
               ),
             )
    ),
    
    tabPanel(
      "Sociodemographics",
      fluidRow(
        style = "margin: 6px;",
        h1(strong("Sociodemographic Characteristics"), align = "center"),
        p("", style = "padding-top:20px;"),
        column(
          4,
          h4(strong("Who Makes Up Hampton Roads?")),
          p(
            "We used the American Community Census data (ACS) to better understand the population in Hampton Roads and Virginia.  The ACS is a yearly survey conducted by the U.S. Census Bureau provides detailed demographic information about American household. We collected the 5-year estimates over the period 2010-2019 to compute the percent of Hampton Roads residents in each locality by race and age.  This information is also presented for the state of Virginia."
          ),
          p(
            "The black population accounts for about 30% of the total population in Hampton Roads. This is significantly greater than Virginia’s which is about 19%."
          ),
          p(
            "The age composition for the Hampton Roads region appears to be representative of Virginia's population. However, there are variations by localities. For example, there exists a large population of young adults (ages 18 to 29) in Southampton County, whereas Portsmouth has a larger population of seniors - individuals 65 years and older. This data suggests that such ages difference may play a role in the differences in economic or education indicators across localities."
          ),
        ),
        column(8,
               tabsetPanel(
                 # Tab race
                 tabPanel("Race",
                          fluidRow(
                            h1(strong("Racial Demographics"), align = "center"),
                            column(
                              6,
                              h4("Hampton Roads"),
                              
                              selectInput(
                                "hampRaceYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              withSpinner(plotOutput("hamp_pie")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B02001"
                              )),
                            ),
                            
                            
                            column(
                              6,
                              
                              h4("Virginia"),
                              
                              selectInput(
                                "VaRaceYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              withSpinner(plotOutput("va_pie")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B02001"
                              ))
                            )
                            
                          )),
                 #Tab age
                 tabPanel("Age",
                          fluidRow(
                            h1(strong("Age Composition of Hampton Roads"), align = "center"),
                            column(
                              6,
                              
                              h4("Hampton Roads"),
                              selectInput(
                                "HampAgeYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              
                              withSpinner(plotOutput("hamp_graph")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B01001"
                              ))
                            ),
                            
                            column(
                              6,
                              
                              h4("Virginia"),
                              p(""),
                              selectInput(
                                "VaAgeYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              
                              withSpinner(plotOutput("va_graph")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B01001"
                              ))
                            ),
                            
                            column(
                              12,
                              
                              h4("Hampton Roads Age Group by Localities"),
                              selectInput(
                                "HampCountAgeYearDrop",
                                "Select Year:",
                                width = "100%",
                                choices = c(
                                  "2019",
                                  "2018",
                                  "2017",
                                  "2016",
                                  "2015",
                                  "2014",
                                  "2013",
                                  "2012",
                                  "2011",
                                  "2010"
                                )
                              ),
                              withSpinner(plotOutput("age_map", height =
                                                       "600px")),
                              p(tags$small(
                                "Data Source: ACS 5 Year Estimate Table B01001"
                              )
                              )
                            )
                            
                          )
                 )
               )
        )
      )
    ),
    
    tabPanel(
      "Data and Methodology",
      fluidRow(
        style = "margin: 6px;",
        h1(strong("Data and Methodology"), align = "center"),
        p("", style = "padding-top:10px;"),
        column(
          4,
          img(src = 'acs.png', style = "display: inline; float: left;", width = "200px"),
          p(
            "The ",
            strong("American Community Survey (ACS)"),
            " is an ongoing annual survey conducted by the U.S. Census Bureau.
                                            The ACS samples households to compile a 1-year and 5-year datasets providing information on households sociodemographic
                                            and socioeconomic characteristics. We used almost a decade (2010-2019) 5- year estimates to obtain county and city level
                                            characteristics to explore the population of Hampton Roads."
          ),
          br(),
          img(src = 'doe.jpg', style = "display: inline; float: left;", width = "150px"),
          p(
            "The ",
            strong("Virginia Department of Education (VDOE)"),
            "collects a variety of information from all school divisions in Virginia including the total number of educators and graduation rates.
                                            This allowed us to report demographic information for educators across the Hampton Roads region along with on-time graduation rate "
          ),
          br(),
          img(src = 'kidscount.png', style = "display: inline; float: left;", width = "150px"),
          p(
            strong("Kids Count"),
            "is a project of the",
            strong("Annie E. Casey Foundation"),
            "produces an annual report that assesses child well-being in the United States.
                                            We use this data source to present information on short-term suspension by race for the counties and cities in the Hampton Roads region. "
          ),
          br(),
          img(src = 'broadbandnow.png', style = "display: inline; float: left;", width = "150px"),
          p(
            strong("Broadband Now"),
            "is a broadband provider company that compiles the results of the FCC form 477 which requires internet providers to report their coverage to the FCC. The information in these reports regarding internet provider, speed, and location were used in the analysis for this dashboard.
                                            We display the relevant information for the counties and cities in the Hampton Roads region."
          ),
          br(),
          img(src = 'oecd.png', style = "display: inline; float: left;", width = "150px"),
          p(
            strong("OECD/INFE 2020 Financial Literacy survey"),
            "The OECD International Network on Financial Education conducts this survey at an international level and aims to identify the level of financial literacy of the survey respondents. The survey provides information about the respondents (credit scores and race), and creates a financial literacy score based off of the respondents answers to various questions.
                                            We used the information relevant to Virginia to establish an idea of the financial literacy of the Black population compared to other racial groups."
          ),
          br(),
          img(src = 'censusreporter.png', style = "display: inline; float: left;", width = "150px"),
          p(
            strong("Census Reporter"),
            "Census Reporter compiles the information from the American Community Survery for various zip codes. This allows to search information relevant to each zip code within Hampton Roads and establish population characteristics."
          ),
          br(),
          img(src = 'zillow.png', style = "display: inline; float: left;", width = "150px"),
          p(
            strong("Zillow"),
            "Zillow is one of the leading real-estate websites in the United States offering information for selling, buying, and renting homes all across the United States. This website provides the information for the median home value of any given zip code in the Hampton Roads area."
          ),
          br(),
          img(src = 'googleMapLogo.png', style = "display: inline; float: left;", width = "150px"),
          p(
            strong("Google Maps API"),
            "Using a Google Maps Platform developer account, accessing real-time information about places and locations in the globe is incredibly easy. Returned in an easy to format, easy to plot way, the Google Maps API was used to locate food banks and pantries within the Hampton Roads area."
          ),
        ) ,
        
        
        column(
          4,
          h5(strong("Methodology")),
          p(
            "We explore five main pillars to examine the economic and social well-being of the Black community in Hampton Roads."
          ),
          p(
            strong("Education:"),
            "Educational attainment is a major contributor to improving an individual’s economic well-being and standard of living. As such, we examine the disparities in educational attainment to determine the economic progress of the Black community.
                                          Specifically, we collected data on on-time high school graduation rates and bachelor’s degree attainment. We also include the racial/ethnic representation of educators. Research indicates some distinct, albeit small, advantages when black students are paired with a Black teacher [1]. Suspension rate was also included in our analysis as some students are more likely to be disciplined than others even for the same transgression (KIDS Count).
                                          Moreover, a higher suspension rate will limit educational attainment, given the negative effect on achievement [2].
                                            "
          ),
          img(
            src = 'education.png',
            align = "center",
            height = "300px",
            width = "400px"
          ),
          p(
            tags$small(
              "[1] Anna J. Egalitea, A.J. , Kisida B., & Winters, M.A. (2015), Economics of Education Review, 45, 44-52."
            )
          ),
          p(
            tags$small(
              "[2] Noltemeyer, A. L., Ward, R. M., & Mcloughlin, C. (2015). Relationship between school suspension and student outcomes: A meta-analysis. School Psychology Review, 44(2), 224-240."
            )
          ),
          p(
            strong("Economics:"),
            "A major driver of economic growth and mobility is growth in labor, income, health, and wealth accumulation (homeownership). Thus, we examine median income, poverty rates, industry employment and unemployment rates, health insurance status, and homeownership rate for the Black and general population across the counties and cities in Hampton Roads [1]."
          ),
          p(
            "To further assess economic progress, we included several characteristics to measure Black household’s well-being.  We included welfare measures (Food Stamps or SNAP recipients); family stability (female-headed household, marital rates, grandparents’ guardianship, and migration); transportation access (private and public transportation); and technology access (computer and internet access)."
          ),
          p(),
          img(
            src = 'economic.png',
            align = "center",
            height = "300px",
            width = "400px"
          ),
          p(),
          p(),
          p(
            "Lastly, we rank counties/cities based on several of our indicators. The idea is to determine whether the Black community was consistently underperforming in some localities in the Hampton Road region."
          ),
          p(),
          p(
            tags$small(
              "[1] Butler, S. M., Beach, W. W., & Winfree, P. L. (2008). Pathways to economic mobility: Key indicators. Economic mobility project."
            )
          )
        ),
        
        
        column(
          4,
          p(
            strong("Policy & Justice:"),
            "We analyzed the home values for zip codes in the Hampton Roads region using information from Zillow & compared them to the population characteristics of each zip code using Census Reporter. 
            We aim to identify a relationship between the local Black population and local home values."
          ),
          p(
            strong("Media & Entertainment:"),
            "We investigated internet coverage in the area by establishing the maximum number of internet providers in any given zip code.
            The number of providers per zip code indicates how accessible internet is in that area compared to others. 
            On the other hand, it only speaks to potential coverage based on what is available and doesn't describe how many people actually have internet access.
            The internet quality is based on how many providers in the area offer plans with download speeds great enough for the average Black family in Hampton Roads."
          ),
          p(
            # selectizeInput("files", "Select a File", choices = c("Positive Bag", "Negative Bag", "Diversity Bag", "Black Diversity Bag"), selected = "Positive Bag", multiple = FALSE),
            # downloadButton('downloadData', 'Download')
            "Due to lack of quantitative data that exists, we analyzed qualitative data by applying text analytics and natural language processing to evaluate the diversity of Hampton Roads 
            news sources and the intersection between positive and negative connotations of the articles. For the viewer’s evaluation and transparency, the text word banks of positive, negative, 
            diversity, Black diversity are available to download below.",
            uiOutput("tab")
          ),
          p(
            strong("People & Values:"),
            "We investigated financial literacy survey results to describe the financial literacy of the Black population as a whole. The survey provides results from a wide variety of countries,
            here we only focused on the responses from Virginia residents. Unfortunately this information is not available on a more granular level yet, so we aim to draw insights from the most representative
            population available in the data. Another facet of People & Values that is touched on is the availability of food, and the area's food security. Food security allows for a healthier quality of life in all communities. 
            By analyzing what areas are more food insecure than others, along with providing the information and resources to make a change, a community can grow and thrive."
          ),
          
        )
        
      )
      
    )
  ),
  
  
  
  # Education Indicators ----------------------------------------------------
  
  
  navbarMenu(
    title = "Education",
    #Educator Demographics
    tabPanel("Educators",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Who are the Educators in Hampton Roads?")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "The Black population overwhelmingly attends public schools, yet in most parts of the U.S., teachers are overwhelmingly White. The National Center for Education
						Statistics reports that about 79 percent of all U.S. public school teachers were non-Hispanic White [1]. This trend is also reflected in the State of Virginia.
            For the 2020-2021 academic year, the Virginia Department of Education released racial and ethnic breakdown for educators-administrators, teachers, and other pupil personnel- by school division.
						These data enable examination of racial and ethnic representation to be matched within local school divisions.
           In 2020/21, 13 percent of public-school educators identified as Black, compared to a state rate of 19.9% Black population in 2019, a difference of almost seven percentage points [2]. "
                 ),
                 p(
                   "In 2019, 31% percent of the Hampton Roads population identified as Black.  Seven of the 16 localities (44%) reported rates of public school Black educator representation at or above the regional average, while another 7 reported rates less than 12%.
						Black educator representation ranged from high rates of 62.3% in Portsmouth and 55.8% in Franklin City to very low rates of 3.5% in Poquoson.  These percentages are reversed for White educators, with highs of 92.6% representation in Poquoson and lows of 34.6% and 40% respectively in Portsmouth and Franklin City."
                 ),
                 p(
                   "Hampton Roads localities vary considerably in their racial and ethnic profiles, making it difficult to ascertain from these data alone how well educator representation matches specific communities. A future project might be to map local demographic profiles to school division educator representation to determine
						how well Black educators are matched to local Black population representation."
                 ),
                 p("", style = "padding-top:20px;"),
                 p(
                   tags$small(
                     "[1] Taie,S. and Golding, R. (2020). Characterisitcs of Public and Private Elementary and Secondary School Teachers
						in the United States: Results from the 2017-18 National Teacher and Principal Survery. First Look. NCES 2020-142. National Center for Education Statistics"
                   )
                 ),
                 p(
                   tags$small(
                     "[2].  Virginia Department of Education, Virginia Educator Ethnicity and Race Data, https://doe.virginia.gov/teaching/workforce_data/index.shtml; U.S Census QuickFacts, https://www.census.gov/quickfacts/fact/table/VA,US/PST045219."
                   )
                 ),
               ),
               column(
                 8,
                 h1(strong("Educator by Racial and Ethnic Groups"), align = "center"),
                 p("", style = "padding-top:20px;"),
                 selectInput(
                   "teacherRaceBreakdown",
                   "Select Race:",
                   width = "100%",
                   choices = c(
                     "Black",
                     "White",
                     "American Indian",
                     "Asian",
                     "Hawaiian",
                     "Two or More Races",
                     "Hispanic"
                   )
                 ),
                 withSpinner(plotlyOutput("teacherRacePlots")),
                 p(
                   tags$small(
                     "*Note: Missing bar reflects 0 teachers for that locality's racial/ethnic group. Williamsburg and James City educator data were combined from the source dataset. "
                   )
                 ),
                 p(
                   tags$small("Source: Virginia DOE, Virginia 2020-2021 Teacher Race Report")
                 )
               )
             )),
    tabPanel("Educational Attainment",
             fluidRow(
               p("", style = "padding-top:40px;"),
               column(
                 4,
                 h4(strong("Graduating High School On-Time")),
                 p(
                   "Virginia Department of Education defines on-time graduation as the percentage of students who earned a Board of Education-approved diploma within four years of entering high school for the first time. On-time graduation rates have been steadily climbing in Virginia from 2009 thru the 2020 school year moving from 85.5% in 2010 to 92.5% in 2020. Black students have made even greater gains across this period, from 80.4% in 2011 to 91.4% graduating within four years in 2020."
                 ),
                 p(
                   "Across Hampton Roads, the same upward state trend is evident across the decade. The regional average on-time graduation rate increased steadily from 82% in 2010 to 92.6% in 2020. Black students mirrored the trend, from a regional average of 79.7% in 2010 to 92.4% in 2020, effectively matching the total regional and state population average."
                 ),
                 p(
                   "Due to the large variation between localities, we examine local differences.  For Black students, gains ranged from 7.8 to approximately 24 percentage points, with an average improvement of 17 percentage points.  Portsmouth and Southampton made the most dramatic gains, both showing upward trends from lows in 2010 of 66.1% and 72.9% respectively, to highs of 89.6% and 95% in 2020, a similar pattern to Hampton, which also showed very impressive steady gains (from 78% in 2010 to 96% in 2020)."
                 ),
                 p(
                   "While every locality showed improvement over time, two locality rates remained notably lower to the state average. Norfolk and Suffolk remained more than 6.8 and 7.2 percentage points lower than the state average in 2020. Future research could help better understand whether on-time graduation rates reflect students leaving school, obtaining alternate certification, or moving out of state without transfer notice (Suffolk in particular borders North Carolina)."
                 ),
                 
                 h4(strong("College Degree")),
                 p(
                   "A college degree is not only a 'requirement for most jobs', but leads to higher wages, financial security, and improvement in standard of living. In the U.S., there is an increasing trend of college attainment over the last decade. The ACS reports a similar trend for Virginia residents.
                                                       In 2010, 33.8% of Virginia residents aged 25 years or older had earned a bachelor's degree or higher. This rate improved to 38.8% by 2019."
                 ),
                 p(
                   "There are substantial differences in attainment rate for Black residents across the localities in Hampton Roads.  In 2019, four-year college degree rates ranged from a low of 9.8% (Poquoson) to a high of 34.8% (York).
                                                       Additionally, the differential rates between the general population and Black residents vary widely across localities by year and across years.
                                                       Some of the largest gaps are found in 2019 in Williamsburg (39.6 percentage point difference) and Poquoson (33 percentage points). The shortest gap at the end of the decade was in Hampton at 1.2 percentage points."
                 )
               ),
               column(
                 8,
                 h1(strong("Educational Attainment"), align = "center"),
                 tabsetPanel(
                   #On-time Graduation Rate
                   tabPanel("High School Graduation",
                            
                            column(12,
                                   fluidRow(
                                     p("", style = "padding-top:10px;"),
                                     h4(
                                       strong("High School On-Time Graduation Rates in Hampton Roads"),
                                       align = "center"
                                     ),
                                     withSpinner(leafletOutput("dropout_map", width = "850px")),
                                     p(tags$small(
                                       "Data Source: Virginia Department of Education"
                                     )),
                                     
                                   ))),
                   
                   tabPanel(
                     "Bachelor's Degree",
                     p(""),
                     fluidRow(
                       p("", style = "padding-top:40x;"),
                       h4(
                         strong(
                           "Percentage of Hampton Roads Population 25 years and older with Bachelor's degree or higher"
                         ),
                         align = "center"
                       ),
                       selectInput(
                         "genEdAttainmentYearDrop",
                         "Select Year:",
                         width = "100%",
                         choices = c(
                           "2019",
                           "2018",
                           "2017",
                           "2016",
                           "2015",
                           "2014",
                           "2013",
                           "2012",
                           "2011",
                           "2010"
                         )
                       ),
                       withSpinner(plotlyOutput("genEdAttainmentPlots")),
                       p(tags$small("*Note: Data missing for some years. ")),
                       p(
                         tags$small(
                           "Data Source: ACS 5 Year Estimate Tables C15002B (For Black Population), S1501 (For Total Population)"
                         )
                       )
                       
                     )
                   )
                   
                 )
               )
             )),
    
    tabPanel("Suspension",
             
             fluidRow(
               p("", style = "padding-top:60px;"),
               column(
                 4,
                 h4(strong("Who is Being Punished?")),
                 p(
                   "The Virginia Department of Education refers to students being barred from attending school for at most 10 days.  We collected short-term suspension rate by race from Kids COUNT, Annie E. Casey which is defined as the total number of offenders of a certain race divided by the total number of students of that race for each locality. Rates include students from grades K - 12."
                 ),
                 p(
                   "Across Virginia, from academic years 2014-15 to 2018-19, the Black student suspension rate was consistently more than eight percentage points higher than White and Hispanic student rates. Black students were suspended between a relative low of 12.0 % in 2018-19 and a high of 13.6 % in 2016-17. In contrast, the highest suspension rate for White and Hispanic students was 3.8 % and 3.9 % respectively, both in 2016-17."
                 ),
                 p(
                   "Black student suspension rates in Hampton Roads were decidedly higher than for White students across all years examined. This is consistent even for areas with low suspension rates - averaging rate across years, the lowest rate for Black students was 4.9% for Williamsburg-James City County which is in contrast to White students which was 2.5%. On average, the White student suspension rate was 4.8%, almost a 9-point difference."
                 ),
                 p(
                   "To illustrate gaps between Black and White student suspension rates over time, we plot the percentage point difference between Black and White rates for each Hampton Roads locality that reported more than two years of data for students of both races.  Most localities showed a Black-White student suspension gap rate of between 5 and 14 percentage points over this time period. "
                 ),
                 p(
                   "Norfolk consistently showed the greatest racial suspension gap between students, ranging from a relative low of 12.8% in 2016-17 to a high gap of 14.1% in 2018-19.  York showed the smallest gap for three of the five years, followed by Gloucester for two years."
                 )
               ),
               column(
                 8,
                 h1(strong("Short Term Suspension"), align = "center"),
                 tabsetPanel(
                   tabPanel(
                     "Suspension Rate",
                     p(""),
                     p("", style = "padding-top:10px;"),
                     h4(
                       strong("Percent of Students Suspended Short-Term in Virginia"),
                       align = "center"
                     ),
                     withSpinner(plotlyOutput("suspension_line_graph")),
                     p(tags$small(
                       "Data Source: KIDS COUNT, Annie E. Casey Foundation"
                     ))
                   ),
                   tabPanel(
                     "Suspension Rate by Race",
                     p(""),
                     p("", style = "padding-top:10px;"),
                     h4(
                       strong("Percent of Students Suspended Short-Term in Hampton Roads by Race"),
                       align = "center"
                     ),
                     selectInput(
                       "BWsuspensionYearDrop",
                       "Select Year:",
                       width = "100%",
                       choices = c("2019", "2018", "2017", "2016", "2015")
                     ),
                     withSpinner(plotlyOutput("BW_map", height = "700px")),
                     p(tags$small(
                       "Data Source: KIDS COUNT, Annie E. Casey Foundation"
                     )),
                     p(
                       tags$small(
                         "*Note: Black student data supressed for Mathews and Poquoson. Missing data for some years for Franklin for White student rates"
                       )
                     )
                   ),
                   tabPanel(
                     "Suspension Gap",
                     p(""),
                     p("", style = "padding-top:10px;"),
                     h4(
                       strong(
                         "Difference Between Black and White Student Short-Term Suspension"
                       ),
                       align = "center"
                     ),
                     withSpinner(plotlyOutput("suspensionGap", height = "700px")),
                     p(tags$small(
                       "Data Source: KIDS COUNT, Annie E. Casey Foundation"
                     ))
                   )
                   
                 )
                 
               )
               
             ))
  ),
  
  
  navbarMenu(
    title = "Economics",
    #Median Income
    tabPanel(
      "Income",
      fluidRow(
        style = "margin: 6px;",
        p("", style = "padding-top:30px;"),
        #change this later to formatting that is desired
        column(
          4,
          h4(strong(
            "Household's Economic Status in Hampton Roads"
          )),
          p(
            "In 2019, the Black community had a median annual income of $52,596, which is less than the median income of $67,387 of Hampton Road’s general population. This gap is consistent with the state trend, although it is larger at the state level, where the Black and general households earn $51,654 and $74,222, respectively."
          ),
          p(
            "While there are racial differences in median income, the gap between the Black community and the general population has been closing over the last decade. In 2010, the racial median income gap in Hampton Roads was $17,121, whereas it was $14,791 in 2019. This suggests that despite the lower median income, the Black community's economic status has slightly improved over the last decade."
          )
        ),
        h1(strong(
          "Median Income in Virginia and Hampton Roads"
        ), align = "center"),
        column(
          8,
          withSpinner(plotOutput("medianTimeGraph")),
          p(tags$small(
            "Data Source: ACS 5 Year Estimates Table S1903"
          )),
          
          selectInput(
            "MedianIncomeYearDrop",
            "Select Year:",
            width = "100%",
            choices = c(
              "2019",
              "2018",
              "2017",
              "2016",
              "2015",
              "2014",
              "2013",
              "2012",
              "2011",
              "2010"
            )
          ),
          withSpinner(plotOutput("income_plot")),
          p(tags$small(
            "Data Source: ACS 5 Year Estimates Table S1903"
          ))
        )
      )
    ),
    
    #),
    #Homeownership Map
    tabPanel("Homeownership",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Home Owners")),
                 p(
                   "We report homeownership rates in Hampton Roads. The interactive map includes both the percentage of homeowners defined as total homeowners/ total population and the percentage of black homeowners calculated as the total black homeowners/total Black population.
                                         The radio buttons at the top right of the map allow the user to view the percentage of Black homeowners across the localities in Hampton Roads."
                 ),
                 p(
                   "Homeownership improves individuals' long-term wealth. It also improves civic and political engagement, children's educational outcomes, and health behaviors.
                                          As such, we examine the rate of homeownership for the Black community in Hampton Roads."
                 ),
                 p(
                   "Regardless of locality, the Black population has a lower rate of homeownership than the general population. The disparity
                                         is persistent for most counties/cities. In James City, there is a 23 percentage points difference between the general population and
                                         the Black population, with a 76% and 53.1% homeownership rate, respectively. For the other localities, differences range from 5 to 28 percentage points.
                                         Poquoson was the only locality where the Black population had a greater homeownership rate than the general population."
                 )
               ),
               column(
                 8,
                 h1(strong("Home Ownership in Hampton Roads"), align =
                      "center"),
                 withSpinner(leafletOutput("homeownership_map")),
                 p(tags$small(
                   "Data Source: ACS 5 Year Estimates Table S2505"
                 ))
               ),
             )),
    
    
    #Unemployment Rate
    tabPanel("Labor Market",
             fluidRow(
               p("", style = "padding-top:40px;"),
               column(
                 4,
                 h4(strong("Labor Sector & Unemployment")),
                 p(
                   "We examine the economic health of the labor market for the Black communities in Hampton Roads. First, we look at the top two industries that explain the highest number of workers in the region. This allows us to understand the employment landscape of the region, which may provide insights into vulnerable areas.
                                         We then analyze unemployment to understand the economic hardship that households in the Hampton Region face."
                 ),
                 p("", style = "padding-top:10px;"),
                 h5(strong (
                   "Industries that employ the largest number of workers"
                 )),
                 p(
                   "Majority of the residents in the Hampton region were employed in the Educational Services, health care, and social services sector. Among each locality in Hampton Roads, educational services, health care, and social services was one of the top two industries that employ the highest number of workers.
                                         However, for the other top employable industry, there are some slight variations across localities and time.
                                         For example, in 2012, eight of the 16 localities top two sector was in retail trade. However, only two areas had retail trade in the top two industries indicating more economic diversity among the localities over time."
                 ),
                 h5(
                   strong (
                     "Are there differences in the unemployment rate across localities?"
                   )
                 ),
                 p(
                   "The unemployment rate for the Black population in Hampton Roads tends to be greater than the average rate in Virginia, regardless of the time period. For example, in 2019, Franklin-which had the highest unemployment
                                         rate in the region- the unemployment rate for the black community was 14.9 % compared with Virginia's average of 4.6%.  Moreover, regardless of localities, the Black community is more likely to be unemployed than the other residents.  In 2019, the Black population had a higher unemployment rate than their counterparts in every county and city in the Hampton Roads region, even in areas that were below the state's average (Gloucester)."
                 ),
                 p(
                   "While for some counties there has been a decline in unemployment rates across the decade (2010-2019) for Black households, there still exists some significant disparity for most counties/cities. One such example is Williamsburg County that in 2010 had a similar unemployment rate for both Blacks and the region residents, 6.0% and 6.1%, respectively. But by 2019, that gap widen significantly by 4.6 percentage points (the Black unemployment rate was 10.4% and the total population 5.8%)"
                 ),
                 
                 
               ),
               column(
                 8,
                 h1(strong("Labor Market Characteristics"), align = "center"),
                 tabsetPanel(
                   #Sector Employment
                   tabPanel(
                     "Industry Employment",
                     p("", style = "padding-top:10px;"),
                     h4(strong("Top Two Industry Sectors"), align = "center"),
                     fluidRow(
                       p(""),
                       selectInput(
                         "SectorEmploymentYearDrop",
                         "Select Year:",
                         width = "100%",
                         choices = c(
                           "2019",
                           "2018",
                           "2017",
                           "2016",
                           "2015",
                           "2014",
                           "2013",
                           "2012",
                           "2011",
                           "2010"
                         )
                       ),
                       withSpinner(plotlyOutput("sector_plot")),
                       p(tags$small("*Note: Data missing for some years. ")),
                       p(tags$small("Source: ACS 5 Year Estimate Table DP03"))
                       
                     )
                   ),
                   
                   tabPanel(
                     "Unemployment Rate",
                     p("", style = "padding-top:10px;"),
                     h4(strong("Unemployment Rate in Hampton Roads"), align = "center"),
                     withSpinner(plotlyOutput("unemployment_plot")),
                     p(
                       tags$small(
                         "*Note: Red dotted line represents Virgina's unemployment rate. Missing data for the Black population in Mathews and Poquoson"
                       )
                     ),
                     p(tags$small(
                       "Data Source: ACS 5 Year Estimates Table S2301"
                     )),
                     sliderInput(
                       "UnemploymentRateSlider",
                       "Select Year",
                       value = 2019,
                       min = 2010,
                       max = 2019,
                       sep = "",
                       width = "100%",
                       animate =
                         animationOptions(interval = 1400)
                     ),
                   ),
                   tabPanel(
                     "Unemployment Rate Trends",
                     p("", style = "padding-top:10px;"),
                     fluidRow(
                       width = 12,
                       height = 550,
                       img(
                         src = "updated_unemployment_plot.gif",
                         height = "800",
                         width = "1100"
                       )
                       
                     )
                   )
                 )
               )
             )),
    
    
    #Poverty Rate
    tabPanel("Poverty",
             fluidRow(
               p("", style = "padding-top:50px;"),
               column(
                 4,
                 h4(
                   strong (
                     "How does the poverty rate in Hampton Roads compare to all of Virginia?"
                   )
                 ),
                 p("", style = "padding-top:10px;"),
                 p(
                   "It is clear that regardless of location the Black population has borne a higher rate of poverty than other races by about 7-8 percentage points between 2012-2019. Still, Hampton Roads has endured somewhat higher rates over the years. In 2019, 11.3% of people in Hampton Roads were in poverty, 0.7 percentage points higher than in Virginia overall. And among the Black population, the gap (1.2 percentage points) was even wider. In 2012, the gaps were smaller, but since then has gotten wider, indicating a deterioration in economic circumstances, especially for the Black population."
                 ),
                 p("", style = "padding-top:10px;"),
                 h5(
                   strong (
                     "Is poverty more prevalent in some counties or cities in Hampton Roads than others?"
                   )
                 ),
                 p(
                   "It appears that, even at the county or city level, the Black population is always more likely to be in poverty than other residents. The disparity has been pronounced and persistent for most counties/cities. Most notably, Mathews has experienced a poverty gap that has grown from about 6 percentage points in 2012 to 30 percentage points in 2019. On the other hand, some counties/cities have enjoyed a significant decline in poverty rates and gaps over time, notably Franklin City. The city had a poverty rate of roughly 36% among Blacks compared to 23% in its entire population in 2012. But by 2019, the rate decreased markedly for both groups to 22% and 15% respectively, cutting the poverty gap by half over the 8-year period."
                 ),
                 
               ),
               column(
                 8,
                 h1(strong("Poverty Rates in Hampton Roads"), align = "center"),
                 tabsetPanel(
                   tabPanel(
                     "Poverty Rates",
                     p("", style = "padding-top:10px;"),
                     h4(strong(
                       "Poverty Rates across Hampton Roads and Virginia"
                     ), align = "center"),
                     selectInput(
                       "PovertyYearDrop",
                       "Select Year:",
                       width = "100%",
                       choices = c("2019", "2018", "2017", "2016", "2015", "2014",
                                   "2013", "2012")
                     ),
                     withSpinner(plotOutput("pov_plot")),
                     p(tags$small(
                       "Data Source: ACS 5 Year Estimates Table S1701"
                     ))
                   ),
                   tabPanel(
                     "Poverty Rates across Localities",
                     p("", style = "padding-top:10px;"),
                     h4(
                       strong("Poverty Rates across Hampton Roads' Cities and Counties"),
                       align = "center"
                     ),
                     selectInput(
                       "PovertyCountYearDrop",
                       "Select Year:",
                       width = "100%",
                       choices = c("2019", "2018", "2017", "2016", "2015", "2014",
                                   "2013", "2012")
                     ),
                     withSpinner(plotlyOutput("counties_pov")),
                     p(tags$small(
                       "Data Source: ACS 5 Year Estimates Table S1701"
                     ))
                   ),
                   
                   tabPanel(
                     "Poverty Trends",
                     p("", style = "padding-top:10px;"),
                     fluidRow(
                       width = 12,
                       height = 550,
                       img(
                         src = "poverty.gif",
                         height = "800",
                         width = "1200"
                       )
                       
                     )
                   )
                   
                   
                 )
                 
               )
             )),
    
    #Uninsured Rates
    tabPanel("Health",
             fluidRow(
               p("", style = "padding-top:50px;"),
               column(
                 4,
                 h4(strong("Insurance Status In Hampton Roads")),
                 p(
                   "Health insurance is critical for preventative and emergency care and is considered a key component of financial security.  Since the Patient Protection and Affordable Care Act (ACA) passed in 2010, uninsured rates have fallen across the country. Similarly, the percentage rate for Virginias dropped from 12.3% in 2010 to 8.6% in 2019.
                                              The rates for Black Virginians were higher - in 2012, 15.4% were uninsured, whereas, in 2019, it was 10.1%. "
                 ),
                 p(
                   "The Black population of Hampton Roads showed similar trajectories to the national and state trends, with most localities showing a fairly linear falling trend.  The most dramatic change occurred in the later years, particularly in 2018 and 2019, when several localities almost or more than halved their Black uninsured rate.
                                              Most dramatically, in Franklin City, more than a quarter of the Black population (25.8%) was uninsured in 2012. By 2019, 7.3% were uninsured, a difference of 18.5 percentage points. "
                 ),
                 p(
                   "While almost all localities showed declines, many rates remained higher than the state average.
                                              Norfolk consistently remained above the state average (20.7% uninsured in 2012 to 13.8% in 2019) despite a decreasing trend. On the other hand, Chesapeake and Virginia Beach consistently had lower rates than state averages. York had by far the lowest rates over time for an area with proportional Black representation."
                 ),
                 p(
                   "Several exceptions to the general pattern also emerged. Mathews showed an increasing trend of Black residents uninsured, ranging from a relative low of 18.7% in 2013 to the highest level in 2019 at 32.4%. In stark contrast, all Black residents of Poquoson were consistently insured across the decade. Such differences may be due to the areas' small Black population. "
                 ),
                 p(
                   "Several important policy changes, as well as economic improvement following the Great Recession, may explain some of the patterns.  Full implementation of the ACA started in 2014, and in 2018 Virginia expanded Medicaid under the ACA.  Thus, there may be further improvements in future data."
                 )
                 
               ),
               column(8,
                      fluidPage(
                        h1(strong("Health Uninsured Rates"), align = "center"),
                        withSpinner(plotlyOutput("uninsured_plot")),
                        sliderInput(
                          "UninsuredPctSlider",
                          "Select Year",
                          value = 2019,
                          min = 2012,
                          max = 2019,
                          sep = "",
                          width = "100%",
                          animate = animationOptions(interval = 1400)
                        ),
                        p(
                          tags$small("*Note: Data missing for Black population in Poquoson.")
                        ),
                        p(tags$small(
                          "Data Source: ACS 5 Year Estimates Table S2701"
                        ))
                      ))
               
             )),
    
    
    #Veteran Status
    
    tabPanel("Veterans",
             fluidRow(
               p("", style = "padding-top:50px;"),
               column(
                 4,
                 h4(strong("Who has served in Hampton Roads?")),
                 p(
                   "Hampton Roads is rich in military bases and establishments. All branches of the U.S. military have bases or substantial presence, with Naval Station Norfolk being the largest naval station in the United States, if not the world [1].  Starting in the 1960s, Black Americans have been overrepresented in the United States Armed Services relative to their proportion of the population [2].
                                       Among other benefits, many view the military as an economically cost-effective means to gain training and educational advancement.   "
                 ),
                 p(
                   "We report rates of Black veteran status as a percentage of the total Black population by locality. Percentages were calculated by dividing the Black veteran population by the total Black population for each locality.
                                       The radio buttons at the top right of the map allow the user to view military bases across Hampton Roads. "
                 ),
                 p(
                   "We report rates of Black veteran status as a percentage of the total Black population by locality. Percentages were calculated by dividing the Black veteran population by the total Black population for each locality.  The radio buttons at the top right of the map allow the user to view military bases across Hampton Roads."
                 ),
                 p("", style = "padding-top:40px;"),
                 p(
                   tags$small(
                     "[1]  Hampton Roads Chamber of Commerce, 2021. Retrieved from:",
                     a(
                       href = "https://www.hrchamber.com/page/our-military/",
                       "https://www.hrchamber.com/page/our-military/",
                       target = "_blank"
                     )
                   )
                 ),
                 p(
                   tags$small(
                     "[2]. Ferguson, P. (2021).  U.S. Army. Retrieved from:",
                     a(
                       href = "https://www.army.mil/article/243604/african_american_service_and_racial_integration_in_the_u_s_military",
                       "army.mil/article/243604/african_american_service_and_racial_integration_in_the_u_s_military",
                       target = "_blank"
                     )
                   )
                 ),
                 
               ),
               column(8,
                      fluidPage(
                        h1(strong("Veteran Status in Hampton Roads"), align = "center"),
                        withSpinner(leafletOutput("veteran_map")),
                        p(tags$small(
                          "Data Source: ACS 5 Year Estimates Table S2101"
                        )),
                        sliderInput(
                          "VeteranSlider",
                          "Select Year:",
                          value = 2019,
                          min = 2010,
                          max = 2019,
                          sep = "",
                          width = "100%",
                          animate = animationOptions(interval = 1200)
                        )
                      ),)
             )),
    
    tabPanel("Household Wellbeing",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Household Well-being Characteristics")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "We retrived several indicators using the 2019 5-year ACS estimates to examine the well-being of Black households in Hampton Roads. These characteristics
                                               are presented using interactive maps. Clicking on the tabs and selecting a variable populates the map. Hovering over the map displays the percentage of Black residents living
                                                in a specific city/county with the selected characteristics. "
                 ),
                 p("", style = "padding-top:20px;"),
                 tags$ul(
                   tags$li(("The interactive map shows:")),
                   p("", style = "padding-top:20px;"),
                   withSpinner(textOutput("description_text"))
                 )
               ),
               column(8,
                      fluidPage(
                        h1(strong("Household Characteristics"), align = "center"),
                        selectInput(
                          "select_wellbeing",
                          "Select Indicator:",
                          width = "100%",
                          choices = c(
                            "Percent of Black Households Receiving Foodstamps/SNAP Benefits",
                            "Percent of Black County Migration",
                            "Percent of Black Population that uses car/truck/van to get to work",
                            "Percent of Black Population that uses public transportation to get to work",
                            "Percent of Black Households with a computer with broadband internet",
                            "Percent of Black Households without a computer"
                          )
                        ),
                        withSpinner(leafletOutput("wellbeing_maps")),
                        p(
                          tags$small(
                            "Data Source: ACS 5 Year Estimates Tables: S0901, S2201, S0701, S1002, S1201, S0802, S2802"
                          )
                        ),
                        
                      ))
             ))
    
  ),
  
  # Policy and Justice
  navbarMenu(
    title = "Policy/Justice",
    tabPanel("Traffic Stops",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Traffic Stops")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "In the period of July 1, 2020, to September 30, 2021, a traffic stop was pulled describing the driver’s racial background, age, gender, the severity of violation/action, and time of the incident. From these regions, excluding missing/incomplete data, there were over 16,000 traffic stops across the region, which averages roughly 35 stops daily. In this time span, 8642 African Americans have been recorded in the data, and 7656 Caucasians were the other majority with 219 who identified as Asian/Pacific Islander and 20 who identified as American Indian. The next visualizations will be further in-depth with county/city data on traffic stops identified by their age and racial background. Poquoson data is not on these graphs as their data was incomplete and not sufficient to be used in the matter as the other jurisdictions."),
                 p("", style = "padding-top:10px;"),
                 p(
                   "As in the graphs below, that reveal the number of citations and arrests made, the data vary between jurisdictions and it provides no clear answer on how the traffic stops provide insight on how the black community in the Hampton Roads region is treated. Future projects include additional information on traffic stops such as broadening the field of reasons for stops, researching other databases for matches or inconsistencies, and mapping high-interest locations for officers to be in position for stops."
                 )),
               column(8,
                      fluidPage(
                        h1(strong("Traffic Stops"), align = "center"),
                        tabsetPanel(
                          #Sector Employment
                          tabPanel(
                            "Race Counts",
                            p("", style = "padding-top:10px;"),
                            h4(strong("Race Counts"), align = "center"),
                            fluidRow(
                              p(""),
                              withSpinner(plotOutput('trafficRace')))),
                          tabPanel(
                            "Race and Jurisdiction",
                            p("", style = "padding-top:10px;"),
                            h4(strong("Race and Jurisdiction"), align = "center"),
                            fluidRow(
                              p(""),
                              withSpinner(plotOutput('jurisdiction')))),
                          tabPanel(
                            "Toggle Jurisdictions",
                            p("", style = "padding-top:10px;"),
                            h4(strong("Race Counts"), align = "center"),
                            fluidRow(
                              p(""),
                              selectInput(
                                "select_stop",
                                "Select Hampton Roads County:",
                                width = "100%",
                                choices = c("CHESAPEAKE", "FRANKLIN CITY", "HAMPTON",
                                            "NEWPORT NEWS", "NORFOLK", "POQUOSON",
                                            "PORTSMOUTH", "SUFFOLK", "VIRGINIA BEACH",
                                            "WILLIAMSBURG", "GLOUCESTER CO", "ISLE OF WIGHT CO",
                                            "JAMES CITY CO", "MATHEWS CO", "SOUTHAMPTON CO",
                                            "YORK CO")
                              ),
                              withSpinner(plotOutput('jurisdiction2'))
                            )
                          ))
                      )))),
    tabPanel("City Council Demographics",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("City Council Demographics")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "It is evident that there are a majority of male-dominated city councils and no female majority dominated city councils.  In addition, these plots depict that each city council has at least one female and Newport News and Norfolk genders are equally represented.  Furthermore,  it is evident that half of the city councils have equal representation or majority-black, members.  There are areas such as Chesapeake, Virginia Beach, and Williamsburg that are dominantly white members on the city council where only Williamsburg has only white members."
                 )
               ),
               column(8,
                      fluidPage(
                        h1(strong("City Council Demographics"), align = "center"),
                        withSpinner(plotOutput('cityd')), 
                        withSpinner(plotOutput('cityd2'))))
             )),
    tabPanel("Incarceration Trends",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Incarceration Trends")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "The first graphic depicts the Jail rate per 100,000 people ages 15-64 for the cities and counties within Hampton Roads. These cities and counties are Gloucester County, Isle of Wight County, James City County, Mathews County, Southampton County, York County, Chesapeake City, Franklin city, Hampton city, Newport News city, Norfolk city, Poquoson city, Portsmouth city, Suffolk city, Virginia Beach city, Williamsburg city. The trend observed is that from 1990 to 2006 the jail rate for black people steadily increased from 1,000 to almost 1,700. After 2007 the jail rate steadily decreased back towards 1,000 which translates to for every 100,000 black people in Hampton Roads 1,000 of them are put in jail.  "), 
                 p("The next graphic depicts proportions for demographics for jail population and total population for Hampton Roads in 2018. Note the dashboard allows users to select years 2014 through 2018 however the resulting proportions are very similar. The graphic depicts the underlying systemic issue where black people are 33% of the total population and also makeup 58% of the jail population. Jails are used by local governments for short-term holding and less serious offenses and thus based on the data gathered black people in Hampton Roads are more likely to be dealing with smaller offenses. Some examples of common small offenses are drug charges, offenses against the justice system, traffic offenses, assault, fraud, and theft."),
                 p("The last graphic depicts the Prison rate per 100,000 people ages 15-64 for the cities and counties within Hampton Roads for the year 2013 (most recent data found). This graphic depicts that Williamsburg City, Southampton, and Portsmouth have the highest prison admission rates per 100,000 people in their respective geographic boundaries. Generally, prisons are utilized in increased serious crime offenses than jail which leads to the similarities and differences in prison admission rates over Hampton Roads to be an area for further study.  "),
                 p("From the Policy/Justice pillar, we can conclude that black people in Hampton Roads remain the most incarcerated racial group.  While we can observe a drop in jail rates for black people closer to the rate in 1990, the data available lacks the certainty to conclude a quantifiable extent to which the decrease in jail rate is meaningful for the Hampton Roads community.  We propose that a more important metric is the proportion of which black people make up the total population and the proportion of black people that make up prison and jail populations.  From our analysis, the ratio of both proportions has not significantly changed in the past 5 years.  ")
                 
               ),
               column(8,
                      fluidPage(
                        h1(strong("Incarceration Trends"), align = "center"),
                        tabsetPanel(
                          #Sector Employment
                          tabPanel(
                            "Jail Rates",
                            p("", style = "padding-top:10px;"),
                            h4(strong("Jail Rates"), align = "center"),
                            fluidRow(
                              p(""),
                              selectInput(
                                "select_jailChoice",
                                "Select:",
                                width = "100%",
                                choices = c(
                                  "Virginia",
                                  "Hampton Roads"
                                )
                              ),
                              withSpinner(plotOutput("jail")),
                              p(tags$small("Source: Vera Institute of Justice"))
                              
                            )
                          ),
                          tabPanel(
                            "Jail/Pop Demographics",
                            p("", style = "padding-top:10px;"),
                            h4(strong("Jail and Total Population Hampton Roads Demographics"), align = "center"),
                            fluidRow(
                              p(""),
                              selectInput(
                                "select_pieYear",
                                "Select Year:",
                                width = "100%",
                                choices = c("2014", "2015", "2016", "2017", "2018")
                              ),
                              # showOutput("pie_plots1", "Highcharts"),
                              # showOutput("pie_plots2", "Highcharts"),
                              column(width=6, h3('Jail Population'),
                                     withSpinner(highchartOutput("pie_plots1"))),
                              column(width=6, h3('Total Population'),
                                     withSpinner(highchartOutput("pie_plots2"))),
                              p(tags$small("Source: Vera Institute of Justice"))
                            )
                          ),
                          tabPanel(
                            "Prison Rates",
                            p("", style = "padding-top:10px;"),
                            h4(strong("Prison Rates"), align = "center"),
                            fluidRow(
                              p(""),
                              selectInput(
                                "select_prisonYear",
                                "Select Year:",
                                width = "100%",
                                choices = c("2013", "2012","2011", "2010", "2009")
                              ),
                              withSpinner(leafletOutput("prison")),
                              p(tags$small("Source: Vera Institute of Justice"))
                              
                            )
                          )
                        )
                      ))
             )),
    tabPanel("Gentrification",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Analyzing Gentrification in Hampton Roads")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "Using  manually collected data regarding the average home value from last year by zip code in Hampton roads, the percentage of white population and the percentage of black population from the Five-year-plan. We found that all the zip codes that are all significantly more white than black. Also, the five zip codes with the lowest home values have a higher percent black population than white. For zip codes where houses are worth more than $400,000, the percentage of white residents was greater than the percentage of black residents. We also found zip codes for houses with a value of less than $200,000. The proportion of black residents in all but one zip code 23163 is greater than the proportion of white residents. These data give us a sense of the difference in value between predominantly black and predominantly white areas. Properties receive an appraisal value lower than the contract price in minority tracts compared to those in white tracts."
                 )
               ),
               column(8,
                      fluidPage(
                        h1(strong("Median Home Value & Percent Populations"), align = "center"),
                        withSpinner(leafletOutput("map_homevalues")),
                        p(tags$small(
                          "Data Source: Manually collected from Zillow & Census Reporter (2022)"
                        )))
               ))
    )),
  
  # Media and Entertainment
  navbarMenu(
    title = "Media/Entertainment",
    tabPanel("Internet Coverage",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Coverage of Broadband Internet in Hampton Roads")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "As an initial observation, the maximum number of internet providers (for any given zip code)  in 2015 was 6. The maximum number of providers jumped to 11 in 2020, which can be seen in the 2020 map. One might assume that the zip codes with lower population densities are the ones seeing lower provider numbers. Based on county population totals this seems to be the case, however there are several exceptions of this. As a point of reference, the zip code with the highest population of 10,194 is 23664 (Hampton). The zip code with the lowest population of 96 is 23045 (Mathews). There are several zip codes within Mathews county with very low population density that have relatively high counts of internet providers. As mentioned in preliminary results, this could have something to do with increased efforts to provide “farmhouse” connections to rural areas. As noted in the plots, the 23185 zip code of James City saw one of the highest jumps in provider counts, coming from 5 total providers in 2015 to 11 in 2020."
                 ),
                 h4(strong("Coverage in Historically Redlined Neighborhoods")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "In Hampton Roads, the localities of Norfolk and Newport News experienced redlining during the 1940s when discriminatory housing laws and practices were still enforced. The Home Owners Loan Corporation (HOLC) was a federal program that essentially graded popular American cities by neighborhood. Real estate appraisers would evaluate the economic value of a community based on racial and cultural value. This 'redlining' played a key role in preventing Black families from developing wealth by limiting access to mortgage financing and thus blocking their path to owning property. The practice was not deemed illegal until 1968."
                 ),
                 h4(strong("Limitations")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "Note to anyone who references this information, the data are collected from the FCC form 477 which is how internet providers report their coverage to the FCC. There exists an issue of overstating coverage (by number of providers) due to the manner in which the data are collected. Currently, Form 477 counts an entire census block as “covered” if as little as one address in that area is serviced by a provider."
                 )
               ),
               column(8, # broadbandNow
                      fluidPage(
                        h1(strong("Number of Internet Providers Per Zip Code"), align = "center"),
                        selectInput(
                          "select_coverage",
                          "Select Year:",
                          width = "50%",
                          choices = c("2015", "2020")
                        ),
                        withSpinner(leafletOutput("internet_coverage_maps")),
                        p(tags$small(
                          "Data Source: Sourced from BroadbandNow(2022)" # 
                        )))
               ))
             
    ),
    tabPanel("Internet Quality",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Quality of Broadband Internet in Hampton Roads")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "BroadbandNow defines some standards of internet speeds for a variety of situations. For example, to support “average” internet speed for 3-4 users at once, download speeds of at least 100 Mbps. are required. According to 2020 American census data, the average Black Family household consists of about 3.38 people. That means, the average Black family household would require internet with at least 100 Mbps. download speed to experience “average” speeds. This suggests that if one person would be gaming online for instance it could impact the internet speed of other users in the household especially if everyone is using it at once. Therefore, we focused mostly on identifying what zip codes have access to internet download speeds of at least 100 Mbps."
                 ),
                 p(
                   " As shown here,few providers offered such high quality internet speeds in 2015. The majority of zip codes that did offer such speeds were concentrated in the more densely populated southeastern counties of Hampton Roads. As seen in the maps, the number of providers offering quality internet speeds increases to a maximum of 5. The region overall sees an increase in access to providers with quality speeds where many zip codes that were previously blue (1-2 providers with quality speed) in 2015 change to green (3-4 providers) in 2020 suggesting that there must have been an increase in the demand for this level of internet quality. One obvious reason for such increase in demand is the effect of the COVID-19 pandemic on work and school. Suddenly, many people were forced to continue working and attending school using web based communication systems such as Zoom. Of course, we are still trying to understand the implications of such an event, but based on the evidence gathered here we can assume that there has been an attempt to meet that demand for high quality internet."
                 ),
                 h4(strong("Quality of Internet in Historically Redlined Neighborhoods")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "In an attempt to identify “digital” redlining, we investigated any correlation between those neighborhoods marked as hazardous (in red) and number of internet providers per zip code. While the redlined areas do see comparatively lower internet provider counts than the surrounding areas. However, the level at which it affects the community remains unclear."
                 ),
                 h4(strong("Limitations")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "Note to anyone who references this information, the data are collected from the FCC form 477 which is how internet providers report their coverage to the FCC. There exists an issue of overstating coverage (by number of providers) due to the manner in which the data are collected. Currently, Form 477 counts an entire census block as “covered” if as little as one address in that area is serviced by a provider."
                 )
               ),
               column(8, # broadbandNow
                      fluidPage(
                        h1(strong("Number of Internet Providers Offering Speeds >= 100mbps"), align = "center"),
                        selectInput(
                          "select_quality",
                          "Select Year:",
                          width = "100%",
                          choices = c("2015", "2020")
                        ),
                        withSpinner(leafletOutput("internet_quality_maps")),
                        p(tags$small(
                          "Data Source: Sourced from BroadbandNow(2022)" # 
                        )))
               ))
             
    ),
    tabPanel("News Anchors",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("News Anchors")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "Throughout the Hampton Roads region, there are three major local news channels: WAVY, WTKR, and Channel 13 News. Here we are looking at the race of the news anchors by gender, role, and channel. From the three news channels, there are a total of 90 news anchors. By looking at the pie chart, we can see that the majority of the news anchors (70%) were White and a quarter of the anchors (24.4%) were Black. The other 5% were either of Latin or Asian descent."
                 ),
                 p("", style = "padding-top:10px;"),
                 p(
                   "By looking at the graphic that displays Gender vs. Ethnicity, we can see that there were more Black female anchors than Black male anchors. This opposes the pattern that is displayed by the White anchors. This could be due to more Black females being in that profession in the area."
                 ),
                 p("", style = "padding-top:10px;"),
                 p(
                   "By looking at the graphic displaying Ethnicity vs. Role, we can see that the two most concentrated positions are news anchors, reporters, and meteorologists. Black news anchors were not that many less than the White news anchors, however there seems to be around 3 times more White news reporters and meteorologists than Black news reporters and meteorologists. By looking at the greater race distribution of Hampton Roads, the roles at the news channels compared to race are not a good representation of the community."
                 ),
                 p("", style = "padding-top:10px;"),
                 p(
                   "The last graphic shows Ethnicity Distribution by News Channel. Again, the most hired anchors are White across all channels, however WTKR has the least amount of Black news anchors. Another thing to notice is that WTKR has no anchors from any specific race group besides White and Black. The most racially inclusive channel seems to be Channel 13 based on the data that was collected."
                 )
               ),
               column(8,
                      fluidPage(
                        h1(strong("News Anchors"), align = "center"),
                        withSpinner(plotOutput("anch_plots")),
                        p(tags$small(
                          "Data Source: Manually Collected"
                        )))
               ))
    ),
    tabPanel("Radio Stations",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Radio Stations")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "What is played on the radio and where radio stations are located are telling of the community and what motivates the community. However, it is important to take into consideration that just because certain genres are usually dominated by a specific group/race, that doesn’t mean that only that race listens to that type of music. Due to those reasons, we are focusing on where the radio stations are located. The cities/counties where the most radio stations are located usually indicates the most racially diverse neighborhoods."
                 )
               ),
               column(8, # radio
                      fluidPage(
                        h1(strong("Radio Stations"), align = "center"),
                        withSpinner(leafletOutput("radio")),
                        p(tags$small(
                          "Data Source: Collected from new station" # 
                        )))
               ))
    ),
    tabPanel("Headquarter Locations", #This tab should have the headquarters analysis and graph
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Headquarter Locations ")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "To make up for the lack of quantitative data that exists for the media, entertainment, arts, and technology pillar, we seeked our news articles to 
                   apply text analytics and Natural Language Processing to quantify our data. Black BRAND’s 150 Year Plan recalls that under this pillar, there is a 
                   significant gap in the ownership, representation, and narrative/story/portrayal of media. By finding representation in media that revolves around 
                   entertainment, arts, and technology, we can bridge the gap using our collection of news articles across the region to portray a story using the 
                   positive or negative sentiment in these texts."
                 ),
                 p("", style = "padding-top:10px;"),
                 p(
                   "The Hampton Roads Region has major news channels with online articles along with several more regional online newspapers. The larger news channels 
                  based out of Norfolk do cover a lot more articles related to our pillar of entertainment, media, arts, and technology with more manpower, whereas some 
                  publications talked about more lifestyle content. It should be noted that some of the newspapers did cover numerous regions with its headquarters in 
                  one place and smaller regions of Hampton Roads shared publications, with its headquarters in the bigger city. For example, Smithfield Times based in 
                  Smithfield covered the Isle of Wight and Surry Counties. Tidewater News covers the areas of Franklin, Southampton County, and Isle of Wight County but 
                  has its headquarters based in Franklin. The Gazette Journal based in Gloucester also covers Mathews. To cover articles than span across multiple 
                  regions of Hampton Roads, we did have to go past a paywall to obtain more diverse data. Any articles with its headquarters outside of the Hampton Roads 
                  area were from Virginia news websites that happened to write about the region still. While we tried to find an equal amount of sources per region, it 
                  was still difficult when some publications lacked in articles about entertainment, media, technology, or arts."
                 ),
               ),
               column(8, # put in headquarters graph here
                      fluidPage(
                        h1(strong("Headquarters"), align = "center"),
                        withSpinner(plotOutput("headquarters_graph")),
                        p(tags$small(
                          "Data Source: Newspaper Headquarter Locations" # 
                        )))
               ))
    ),
    tabPanel("Ratio of Sentiment Over Time", #Analysis of ratio graphs on left
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Ratio of Sentiment By Year")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "With the large sum of articles revolving around the media pillar representing the Hampton Roads region, we finally had enough text to be parsed by 
                   our algorithm to fall under the word bank categories of positive, negative, and diversity. Our algorithm not only can parse single words from the 
                   articles’ texts, but also phrases that fall under our diversity word bank. This interactive visualization aims to see how the sentiment of positive 
                   and negative and its ratio changes over the years as our data has articles from as early as 2010. It is important to notice this progress over the 
                   years to see if the work that has been done for the region is effective or if a narrative was being created that conveyed there are still some work 
                   to be done. While there was a large spike of diversity in one year that dropped, it still stayed relatively stagnant. The negative sentiment in the 
                   articles decreased over the years while positive sentiment increases even as diversity remains. It appears that with the existing articles revolving 
                   around this pillars shows that media of the region is relatively positive. "
                 ),
                 h4(strong("Limitations")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "Note to viewers and anyone who references this information, we have web scraped the diversity, positive, and negative word banks from online. We 
                   attempted as much as we could to well as containing our word bank to focus on racial and ethnic diversity. While diversity is intersectional, we 
                   believe that focusing on racial and ethnic diversity aligns best with Black BRAND’s 150 Year Plan. To prevent misleading the audience and keep things 
                   as transparent as possible, we have added our word banks under the “Data and Methodology” tab under “Overview.” In addition, these article prevent 
                   biases are exclusively about media, entertainment, arts, and technology. It is possible that we could have recorded sentiment of overall media in the 
                   region but would have been difficult with the given timeframe. "
                 )
               ),
               column(8, # drop down menu that gives different ratio bar graphs here
                      h1(strong("Sentiment Terminology Over Time"), align = "center"),
                      selectInput(
                        "select_sent_year",
                        "Select Year:",
                        width = "100%",
                        choices = c(
                          "2022",
                          "2021",
                          "2020",
                          "2019",
                          "2018",
                          "2017",
                          "2016",
                          "2015",
                          "2014",
                          "2012",
                          "2010"
                        )),
                      mainPanel(plotlyOutput("sentiment_by_year")),
                      p(
                        tags$small(
                          "Derived from Bag of Words Text Analytics Algorithm"
                        )
                      )
               ))
    ),
    tabPanel("Diversity vs. Positive & Negative",#analysis of div ratio verses pos&neg ratios
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Ratio of Diversity Bag to Positive & Negative Bags")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "The ratios do tell us one part of the story, but to align with the 150 Year Plan we wanted to see if there was an intersection that can tell us a 
                   different story. We can see that most of the articles, regardless of sentiment, fall within the 0.1% diversity ratio. There is also almost an equal 
                   number of positive and negative sentiment articles that have a higher diversity ratio, and this is where we can find some significant information. 
                   While the number of articles are similar, if we the sentiment ratio for each article, we can understand the level of sentiment and how it compares 
                   to the diversity ratio. Overall, the positive sentiment is stronger in positive articles than the negative sentiment in negative articles for the 
                   same diversity ratio. And while we have three data points above the 0.2% diversity ratio (two of which have strong negative sentiment), it would be 
                   imprecise to say that there is more negative sentiment associated with diverse articles. What we can say is that articles from the Hamptons Roads 
                   region have higher positive sentiment than negative sentiment for the same diversity ratios."
                 ),
                 h4(strong("Limitations")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "Note to viewers and anyone who references this information, we have web scraped the diversity, positive, and negative word banks from online. We 
                   attempted as much as we could to well as containing our word bank to focus on racial and ethnic diversity. While diversity is intersectional, we 
                   believe that focusing on racial and ethnic diversity aligns best with Black BRAND’s 150 Year Plan. To prevent misleading the audience and keep things 
                   as transparent as possible, we have added our word banks under the “Data and Methodology” tab under “Overview.” In addition, these article prevent 
                   biases are exclusively about media, entertainment, arts, and technology. It is possible that we could have recorded sentiment of overall media in the 
                   region but would have been difficult with the given timeframe. "
                 )
               ),
               column(8, # graphic of div ratio versus pos&neg ratio
                      fluidPage(
                        h1(strong("Diversity Bag"), align = "center"),
                        withSpinner(plotOutput("div_by_pos_and_neg")),
                        p(tags$small(
                          "Derived from Bag of Words Text Analytics Algorithm" # 
                        )))
               ))
    )),
  
  #People and Values--------------------------------------------------
  navbarMenu(
    title = "People/Values",
    tabPanel("Family Dynamics",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Family Dynamics")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "We retrived several indicators using the 2019 5-year ACS estimates to examine the family dynamics of Black households in Hampton Roads. These characteristics
                                               are presented using interactive maps. Clicking on the tabs and selecting a variable populates the map. Hovering over the map displays the percentage of Black residents living
                                                in a specific city/county with the selected characteristics. "),
                 p("", style = "padding-top:20px;"),
                 tags$ul(
                   tags$li(("The interactive map shows:")),
                   p("", style = "padding-top:20px;"),
                   withSpinner(textOutput("description_famtext"))
                 )
               ),
               column(8,
                      fluidPage(
                        h1(strong("Family Dynamics"), align = "center"),
                        selectInput(
                          "select_family",
                          "Select Indicator:",
                          width = "100%",
                          choices = c(
                            "Percent of Black Children under 18 in Female Head of Household",
                            "Percent of Married Black Population 15 years and over",
                            "Percent of Black Grandparents who are Guardians")
                        ),
                        withSpinner(leafletOutput("family_maps")),
                        p(
                          tags$small(
                            "Data Source: ACS 5 Year Estimates Tables: S0901, S2201, S0701, S1002, S1201, S0802, S2802"
                          )
                        ),
                        
                      ))
             )),
    tabPanel("Religion",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Religion")),
                 p("", style = "padding-top:10px;"),
                 p("The people and values pillar is designed with Maslow's hierarchy of needs in mind. DESCRIPTION OF MASLOWS"),
                 p("", style = "padding-top:10px;"),
                 p( "Are the spiritual needs of citizens in Hampton Roads being met by the churches? We can begin to unpack this question by looking at some religious data from ARDA. Using this source we can picture the religious needs of each county and city."),
                 p("", style = "padding-top:10px;"),
                 p("The plot on the right indicates how prominent a religion is in the cities and counties of Hampton Roads. An NA value, gray on the map, indicates that the database has no information about congregations for that religion in that area. This either happens when there are no congregations in that area for that religion, or when there are congregations in the region but they were not included in the survey (did not respond, weren’t found).")
               ),
               column(8,
                      fluidPage(
                        h1(strong("Religion"), align = "center"),
                        selectInput(
                          "select_rel",
                          "Select Religion:",
                          width = "100%",
                          choices = c('Christianity', 'Judaism', 'Budhism','Hindu', 'Islam')
                        ),
                        withSpinner(leafletOutput("religion")), 
                        p(
                          tags$small(
                            "Data Source: 2010 U.S. Religion Census: Religious Congregations & Membership Study"
                          )
                        )))
             )),
    
    tabPanel("Financial Literacy",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Financial Literacy Score")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "The data we pull from that measures financial literacy is a national survey done by the Federal Reserve. Because most of this data was categorical, there were not many metrics to measure by. Some responses that were interesting are visualized below. All of the data used in this scenario is from Virginia, but is not focused on the Hampton Roads area. This is because the data does not exist publicly at the time of this paper. We see the average scores on the financial literacy assessment portion of this survey. The thickness of each boxplot represents the number of respondents that are of that race. We can see that the average score of the black survey respondents (in blue)  is slightly lower than the average score of the white respondents (in pink)."
                 ),
                 h4(strong("Credit Score Ranking")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "In the credit score tab, we can see the credit score by race. If respondents did not know their credit score, they were able to let the surveyors know that by responding “don’t know”. This plot shows the number of people that have credit scores in each category. Excellent represents a credit score of 800 or more. Good credit scores fall between 740 and 799. Fair credit scores fall between 670 and 739.  Poor credit scores fall between 580 and 669, and very poor credit scores fall between 300 and 579 (Experian). The respondent population is not equal across races, so the counts in this plot could be misleading. However, we can see that the majority of black respondents have a good credit score while the majority of white respondents have an excellent score."
                 ),
                 h4(strong("Knowledge Gap")),
                 p("", style = "padding-top:10px;"),
                 p(
                   "In the last plot we are looking at how many respondents answered “I don’t know” to any of the financial literacy assessment questions. 
                   Black respondents did best in this area, most of them did not respond “I don’t know” throughout the survey."
                 )
               ),
               column(8,
                      fluidPage(
                        h1(strong("Financial Literacy Measured Using VA Survey Data"), align = "center"),
                        tabsetPanel(
                          tabPanel(
                            "Financial Literacy Score",
                            p(""),
                            p("", style = "padding-top:10px;"),
                            h4(
                              strong("Scores by Race"),
                              align = "center"
                            ),
                            withSpinner(plotOutput("financial_literacy")), 
                            p(
                              tags$small(
                                "Data Source: OECD/INFE National Survey"
                              )
                            )),
                          tabPanel(
                            "Credit Score",
                            p(""),
                            p("", style = "padding-top:10px;"),
                            h4(
                              strong("Credit Scores by Race"),
                              align = "center"
                            ),
                            withSpinner(plotlyOutput("credit_scores")),
                            p(tags$small(
                              "Data Source: OECD/INFE National Survey"
                            ))
                          ),
                          tabPanel(
                            "Knowledge Gap",
                            p(""),
                            p("", style = "padding-top:10px;"),
                            h4(
                              strong(
                                "Answered Do Not Know to Any Question by Race"
                              ),
                              align = "center"
                            ),
                            withSpinner(plotlyOutput("dont_know", height = "700px")),
                            p(tags$small(
                              "Data Source: OECD/INFE National Survey"
                            ))
                          )
                          
                        )
                        
                      )))
    ),
    tabPanel("Food Banks",
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Food Banks")),
                 p("Using Google Maps API we were able to collect around 60 different food bank locations within the Hampton Roads area. We did this by choosing three central coordinate points and using a radius search around those three points of 50,000 meters, Google Maps returned a list of locations that met the search criteria. In our case, it was locations that included 'Food Bank' or 'Food Pantry'."),
                 p("After the search was made, we parsed through all of the locations, adding which locality they were in and removed the locations that were outside of the bounds we were searching for."),
                 p("This process yielded a few interesting findings. In the plots to the right we can see the spread of food bank locations in all of the localities."),
                 p("Interestingly enough, both Southampton, and Poquoson did not have any returned food bank locations. Being as big as Southampton is, it was surprising to see nothing in the area in terms of food security resources"),
                 p("We also see that Norfolk and Hampton have the most amounts of food banks, which would mean that higher populated areas, or more food insecure places have more of a focus on providing resources such as food banks or pantries to their communities.")
               ),
               column(8,
                      fluidPage(
                        h1(strong("Food Banks"), align = "center"),
                        tabsetPanel(
                          tabPanel("Food Bank Locations", 
                                   withSpinner(leafletOutput("foodBanksLeaflet")), 
                                   p(
                                     tags$small(
                                       "Data Source: Google Maps API places"
                                     ))),
                          tabPanel("Food Banks in Localities",
                                   withSpinner(plotlyOutput("numFoodBanksLocalities")), 
                                   p(
                                     tags$small(
                                       "Data Source: Google Maps API places"
                                     )))
                        )
                      )))
    ),
    
    
    tabPanel("Food Insecurity: Distance from nearest market", 
             fluidRow(
               p("", style = "padding-top:20px;"),
               column(
                 4,
                 h4(strong("Food Insecurity")),
                 p("Around every five years the U.S. Department of Agriculture releases their Food Access Research Atlas, a comprehensive dataset providing information about food access throughout the nation. Variables including poverty rates, food deserts, and demographic access to sources of food are all provided."),
                 p("We first collected the dataset and cleaned it for better operability within this dashboard by filtering for only Hampton localities. The data is broken into county tracts for a more fine-tuned look."),
                 p("This page is intended to show the poverty rate of each tract in a given locality, along with visualize the African American population's access to supermarkets at two different distances, one and one half mile."),
                 p("Viewing the first map, visualizing poverty rates, we can see a trend that the more densly populated areas, inner city Norfolk, or Hampton, have higher rates of poverty. We can see that it in some tracts reach rates of up to 80.0%."),
                 p("The two subsequent plots visualize the African American communities' access to supermarkets at two different distances. Half mile distances are a lot more prevalent on the outskirts of the Hamton region. Places like Southampton and York Counties have lower access especially seen in the African American Communities"),
                 p("One unfortunate note is the number of N/A's in the one mile distance in Virginia Beach and Norfolk counties."),
                 p("Despite our findings, it is important to note that this does not visualize the accessibility of supermarkets for other demographics. Taking a more holistic view of this data dashboard would be including all demographics and comparing them from there.")
               ),
               column(8,
                      fluidPage(
                        h1(strong("Food Insecurity"), align = "center"),
                        tabsetPanel(
                          tabPanel("Poverty Rates",
                                   withSpinner(leafletOutput("povertyRateMap")),
                                   p(
                                     tags$small("Data Source: US Dept. of Agriculture"
                                     ))),
                          tabPanel("African Population Low Access Markets (Half Mile)",
                                   withSpinner(leafletOutput("lowAccessAF")),
                                   p(
                                     tags$small("Data Source: US Dept. of Agriculture"
                                     ))),
                          tabPanel("African Population Low Access Markets (One Mile)",
                                   withSpinner(leafletOutput("lowAccessAF1")),
                                   p(
                                     tags$small("Data Source: US Dept. of Agriculture"
                                     )))
                        )
                      )))
    )),
  
  tabPanel("Future Work",
           fluidRow(
             p("", style = "padding-top:20px;"),
             column(
               4,
               p("", style = "padding-top:10px;"),
               h4(strong("Future Steps")),
               p("", style = "padding-top:5px;"),
               p(
                 "We provided graphs that rank selected education and economic indicators for each locality in Hampton Roads. We use the 2019 ACS 5-year estimates data to create these rankings. Users can select different indicators that present a bar graph that ranks each locality according to the specific indicator.
                                     We provided this information to determine whether there were some localities in the Hampton Roads region that the Black community was consistently underperforming.  Caution should be noted for the rankings for Poquoson and Mathews County due to the small area size and Black population for both localities.
                                     This has resulted in some outliers and unavailability of data for these two areas for majority of the indicators."
               ),
               p(
                 "The rankings show that there are considerable variations across each county and city in the Hampton region. For instance, Black households seem to be less vulnerable in areas in technology access, welfare assistance, poverty, access to health insurance, and suspension rates in York County. However, they are extremely vulnerable in educational attainment and income/wealth accumulation (that is median income and homeownership).
                                     This trend is similar for all localities. In Chesapeake, there seems to be less vulnerability in areas of health insurance, transportation, technology access, unemployment, and poverty rate. On the other hand, there is a high vulnerability for income/wealth accumulation, welfare assistance, and educational attainment. Given that each jurisdiction in Hampton Roads has a separate municipality, it may be more insightful to analyze the rankings of the indicators within localities rather than making comparisons across localities to inform policymaking.  "
               ),
               p(
                 "The indicators that we have presented in our dashboard, along with the rankings, provide evidence that there are some areas that the Black community needs assistance with to improve economic and social well-being. Data-driven assistance programs in employment, obtaining health insurance, education, homeownership, and improving household stability can foster economic mobility in the Hampton Regions."
               ),
               p(
                 "For this project, we have provided a dashboard that shows a combined view of multiple indicators in two major areas: Education and Economics that collectively addresses the challenges facing the Black communities in the Hampton Region. However, given the substantial variations across localities, a future project would be to consider using the indicators provided to develop an economic and social vulnerability
                                     index specific for the Black community in the Hampton region. This future project could also include the other three pillars: Media, Art, and Entertainment, People and Values, and Politics and Justice to provide a robust and comprehensive index and dashboard for Black BRAND."
               ),
               p(
                 "While the results of the text analysis conducted on local Hampton area newspaper articles and media showed a positive sentiment for the Media/Entertainemnt pillar, it can be expected as new art exhibits and most media exposure garner positive interest, but we hope to expand this type of qualititative analysis to other pillars, like Policy/Justice as this pillar may produce more interesting results. Current news 
                 is not majorly positive in all context, so it would be interesting to see the narrative that is further expemplified by this type of analysis and even how it has changed over time, especially with the start of BlackBRAND's 150 Year Plan. Other notable addiitons that could strengthen the dashboard and relative analysis would be to transition the web-scraping method of online media into a more automatic and reliable
                 manner so that our data does not become obsolete. Lastly, we hope to create a flagging system for words that may have a double meaning, so we remain transparent to BlackBRAND and other users on the context of the analysis that is conducted."
               )
               
             ),
             column(8,
                    fluidPage(
                      h1(strong("Hampton Roads Rankings"), align = "center"),
                      selectInput(
                        "select_rank",
                        "Select Indicator:",
                        width = "100%",
                        choices = c(
                          "Percentage of Black Students 25 yrs and over that have Bachelor's Degree or Higher",
                          "Percentage of Black Students Suspended",
                          "Black Median Income",
                          "Percentage of Black HomeOwners",
                          "Black Unemployment rate",
                          "Black Poverty rate",
                          "Percent of Uninsured (Health) Black population",
                          "Percentage of Black Children under 18 in Female Headed Household",
                          "Percentage of Black Population that uses car/truck/van for work",
                          "Percent of Black Households without a computer"
                        )
                      ),
                      withSpinner(plotlyOutput("ranked_chart")),
                      p(tags$small("*Note: Data missing for some localities ")),
                      p(
                        tags$small(
                          "Data Source: 2019 ACS 5 Year Estimates Tables: C15002B, KIDS Count, S1903, S2505, S2301, S1701, S2701, S0901, S0802, S2802"
                        )
                      ),
                      
                    ))
           )),
  
  navbarMenu(
    title = "Meet the Team",
    tabPanel(
      "VT DSPG",
      value = "team",
      fluidRow(column(3),
               column(
                 6,
                 h4(strong(
                   "Virginia Tech Data Science for the Public Good"
                 ) , align = "center"),
                 p(
                   "The",
                   a(
                     href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html',
                     'Data Science for the Public Good (DSPG) Young Scholars program',
                     target = "_blank"
                   ),
                   "is a summer immersive program held at the",
                   a(
                     href = 'https://aaec.vt.edu/s',
                     'Virginia Tech Department of Agricultural and Applied Economics.'
                   ),
                   "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply, and our annual symposium, please visit",
                   a(
                     href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html',
                     'the official VT DSPG website.',
                     target = "_blank"
                   )
                 ),
                 #p("", style = "padding-top:10px;")
               )),
      fluidRow(
        column(2),
        column(
          2,
          align = "center",
          h4(strong("Graduate Fellow")),
          tags$br(),
          tags$br(),
          img(
            src = "fellow-seth.png",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "45%",
            height = "45%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/aviseth/', 'Avi Seth', target = '_blank'),
            "(Virginia Tech, Computer Science)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("Undergraduate Interns")),
          tags$br(),
          img(src = "BurkholderHeadshot.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          p(
            a(href = 'https://www.linkedin.com/in/matthew-burkholder-297b9119a/', 'Matthew Burkholder', target = '_blank'),
            "(Virginia Tech, Philosophy, Politics, & Economics)"
          ),
          tags$br(),
          img(src = "Mukora.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          tags$br(),
          p(
            a(href = "https://www.linkedin.com/in/victormukora/", 'Victor Mukora', target = '_blank'),
            "(Virginia Tech, Computational Modeling and Data Analytics)"
          ),
          tags$br(),
          img(src = "Christina_Prisbe_Headshot.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          tags$br(),
          p(
            a(href = "https://www.linkedin.com/in/christina-prisbe-60966b218/?midToken=AQGZJw6kSGscnQ&midSig=0iRgRUj5PNWpQ1&trk=eml-email_m2m_invite_single_01-hero-4-prof%7Ecta&trkEmail=eml-email_m2m_invite_single_01-hero-4-prof%7Ecta-null-ec0b8k%7Ekrp4eqqv%7E3v-null-neptune%2Fprofile%7Evanity%2Eview", 'Christina Prisbe', target = '_blank'),
            "(Virginia Tech, Computational Modeling and Data Analytics)"
          ),
          tags$br(),
          img(src = "kwabe.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          tags$br(),
          p(
            a(href = "https://www.linkedin.com/in/kb1999/", 'Kwabe Boateng', target = '_blank'),
            "(Virginia State University, College of Engineering and Technology)"
          ),
          
        ),
        column(
          2,
          align = "center",
          h4(strong("Faculty Advisors")),
          tags$br(),
          img(
            src = "Dr_Holmes.png",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            height = "150px",
            width = "150px"
          ),
          tags$br(),
          p(
            a(href = 'https://aaec.vt.edu/people/faculty/holmes-chanita.html', 'Dr. Chanita Holmes', target = '_blank'),
            "(Virginia Tech, Department of Agricultural and Applied Economics)"
          ),
          tags$br(),
          img(
            src = "Dr_Bradburn.jpg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            height = "150px",
            width = "150px"
          ),
          tags$br(),
          p(
            a(href = 'https://liberalarts.vt.edu/departments-and-schools/department-of-human-development-and-family-science/faculty/isabel-bradburn.html', 'Dr. Isabel Bradburn', target = '_blank'),
            "(Virginia Tech, Department of Human Development and Family Science)"
          ),
          tags$br()
        ),
        
        column(
          2,
          align = "center",
          h4(strong("Stakeholders")),
          tags$br(),
          img(src = "MalloryTuttle.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
          p(
            a(href = 'https://www.linkedin.com/in/mallory-taylor-tuttle/', 'Mallory Tuttle', target = '_blank'),
            "(Associate Director Virginia Tech Hampton Roads Centers)"
          ),
          tags$br()
        )
        
      )
    ),
    tabPanel(
      "CMDA Capstone Fall 21",
      value = "team",
      fluidRow(column(3),
               column(
                 6,
                 h4(
                   strong(
                     "Computational Modeling & Data Analytics Capstone Team (Fall 2021)"
                   ) ,
                   align = "center"
                 ),
                 p(
                   "The",
                   a(
                     href = 'https://data.science.vt.edu/programs/cmda.html',
                     'Computational Modeling and Data Analytics (CMDA) program',
                     target = "_blank"
                   ),
                   "draws on expertise from three primary departments at Virginia Tech with strengths in quantitative science: ",
                   a(href = 'https://math.vt.edu/', ' Mathematics'),
                   ', ',
                   a(href = 'https://www.stat.vt.edu/', 'Statistics'),
                   ', ',
                   a(href = 'https://cs.vt.edu/', ' Computer Science'),
                   '.',
                   "By combining elements of these disciplines in innovative, integrated courses that emphasize techniques at the forefront of applied computation, we teach a rich suite of quantitative skills for tackling today's massive data-based problems."
                 ),
                 p("", style = "padding-top:10px;"),
               )),
      fluidRow(
        column(2),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "sana.jpeg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/sana-abbas-963138187/', 'Sana Abbas', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "talib.jpeg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/talibgrant/', 'Talib Grant', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "caleb.jpeg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/caleb-slaughter-2b904a154/', 'Caleb Slaughter', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "eva.jpeg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/eva-whaley-2022/', 'Eva Whaley', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        )
        
      )
    ),
    tabPanel(
      "CMDA Capstone Spring 22",
      value = "team",
      fluidRow(column(3),
               column(
                 6,
                 h4(
                   strong(
                     "Computational Modeling & Data Analytics Capstone Team (Spring 2022)"
                   ) ,
                   align = "center"
                 ),
                 p(
                   "The",
                   a(
                     href = 'https://data.science.vt.edu/programs/cmda.html',
                     'Computational Modeling and Data Analytics (CMDA) program',
                     target = "_blank"
                   ),
                   "draws on expertise from three primary departments at Virginia Tech with strengths in quantitative science: ",
                   a(href = 'https://math.vt.edu/', ' Mathematics'),
                   ', ',
                   a(href = 'https://www.stat.vt.edu/', 'Statistics'),
                   ', ',
                   a(href = 'https://cs.vt.edu/', ' Computer Science'),
                   '.',
                   "By combining elements of these disciplines in innovative, integrated courses that emphasize techniques at the forefront of applied computation, we teach a rich suite of quantitative skills for tackling today's massive data-based problems."
                 ),
                 p("", style = "padding-top:10px;"),
               )),
      fluidRow(
        column(3),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/emily-sophia-mahr/', 'Emily Mahr', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/allison-woods-vt/', 'Allison Woods', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          tags$br(),
          p(
            "Zhenming Wang (Virginia Tech, CMDA)"
          )
        ))),
    tabPanel(
      "CMDA Capstone Fall 22",
      value = "team",
      fluidRow(column(3),
               column(
                 6,
                 h4(
                   strong(
                     "Computational Modeling & Data Analytics Capstone Team (Fall 2022)"
                   ) ,
                   align = "center"
                 ),
                 p(
                   "The",
                   a(
                     href = 'https://data.science.vt.edu/programs/cmda.html',
                     'Computational Modeling and Data Analytics (CMDA) program',
                     target = "_blank"
                   ),
                   "draws on expertise from three primary departments at Virginia Tech with strengths in quantitative science: ",
                   a(href = 'https://math.vt.edu/', ' Mathematics'),
                   ', ',
                   a(href = 'https://www.stat.vt.edu/', 'Statistics'),
                   ', ',
                   a(href = 'https://cs.vt.edu/', ' Computer Science'),
                   '.',
                   "By combining elements of these disciplines in innovative, integrated courses that emphasize techniques at the forefront of applied computation, we teach a rich suite of quantitative skills for tackling today's massive data-based problems."
                 ),
                 p("", style = "padding-top:10px;"),
               )),
      fluidRow(
        column(3),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "Esha.jpg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'http://www.linkedin.com/in/esha-islam/', 'Esha Islam', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "crystal.jpg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/crystalsulee/', 'Crystal Lee', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "sania.jpg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'http://www.linkedin.com/in/saniamahmood-2022/', 'Sania Mahmood', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "abigail.jpg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/abigail-simpkins-170b1b14a/', 'Abigail Simpkins', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ))),
    tabPanel(
      "CMDA Capstone Spring 23",
      value = "team",
      fluidRow(column(3),
               column(
                 6,
                 h4(
                   strong(
                     "Computational Modeling & Data Analytics Capstone Team (Spring 2023)"
                   ) ,
                   align = "center"
                 ),
                 p(
                   "The",
                   a(
                     href = 'https://data.science.vt.edu/programs/cmda.html',
                     'Computational Modeling and Data Analytics (CMDA) program',
                     target = "_blank"
                   ),
                   "draws on expertise from three primary departments at Virginia Tech with strengths in quantitative science: ",
                   a(href = 'https://math.vt.edu/', ' Mathematics'),
                   ', ',
                   a(href = 'https://www.stat.vt.edu/', 'Statistics'),
                   ', ',
                   a(href = 'https://cs.vt.edu/', ' Computer Science'),
                   '.',
                   "By combining elements of these disciplines in innovative, integrated courses that emphasize techniques at the forefront of applied computation, we teach a rich suite of quantitative skills for tackling today's massive data-based problems."
                 ),
                 p("", style = "padding-top:10px;"),
               )),
      fluidRow(
        column(3),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "john.jpg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/john-malla/', 'John Malla', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "meghna.JPG",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/meghna-banerjee-b6523a91/', 'Meghna Banerjee', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "hyesoo.jpg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/hyesoo-kwon-658b7987/', 'Hyesoo Kwon', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ),
        column(
          2,
          align = "center",
          h4(strong("")),
          tags$br(),
          tags$br(),
          img(
            src = "shashank.jpg",
            style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;",
            width = "85%",
            height = "85%"
          ),
          tags$br(),
          p(
            a(href = 'https://www.linkedin.com/in/shashank-gp-/', 'Shashank Gupta', target = '_blank'),
            "(Virginia Tech, CMDA)"
          )
        ))),
    tabPanel(
      "Team Contributions",
      value = "team",
      fluidRow(column(3),
               column(
                 6,
                 h4(
                   strong(
                     "Team Contributions"
                   ) ,
                   align = "center"
                 ),
                 p("", style = "padding-top:10px;"),
                 p("This dashboard was originally created by the Virginia Tech (VT) DSPG team in the summer of 2021.  The CMDA Capstone Fall 21 team continued their work and added to the 
                   pre-existing material.  The Fall 21 team's contribution includes the work done for the policy/justice (traffic stops, city council demographics & incarceration trends), 
                   media/entertainment (news anchors & radio stations), and people/values pillars (religion). The Spring 2022 Capstone team added onto the pillars of media and entertainment, 
                   policy and justice, & people and values. Emily investigated internet covergae & quality under media and entertainment, Zhenming covered gentrification under policy and justice, 
                   and Allison analyzed financial literacy under people and values.  All of the remaining accrediation for work completed remains with the VT DSPG team and program. The Fall 2022
                   Capstone team expanded the Media/Entertainment pillar with analysis of qualitative data. The Fall 22 team's contributions include text analysis of local newspaper articles and 
                   media through Natural Language Processing to understand the sentiment of Black communities in Hampton Roads media. In the Spring 2023 semester, CMDA's Capstone group expanded upon
                   the people/values pillar by researching food insecurity and food assistance within the Hampton Area. Contributions include detailed maps about poverty rates, food deserts,
                   and food bank locations.")
               ))
    )
  ),
  inverse = T
)

return(ui)