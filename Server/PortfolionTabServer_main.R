# Server side for Portfolio tab



#intially hide everything when no ticker symbols are selected

observeEvent(
  RVs$t,
  {
    if (!length(RVs$t)) {
      hideElement("port_sectionOne_Contents")
      disable("slct_CompareTkrSlct")
      disable("slct_retfreq")
      disable("slct_benchmark_port")
      disable("btn_updateCompareChart")
      
      
      disable("btn_PortSpecfill")
      disable("port_sectionTwo_Optim_slctTkrs")
      disable("port_sectionTwo_Optim_daterange")
      disable("port_sectionTwo_Optim_retfreq")
      disable("port_sectionTwo_Optim_UseMinMax")
      disable("port_sectionTwo_Optim_slct_Rf")
      disable("port_sectionTwo_OptimBtn")
     
    } else {
      showElement("port_sectionOne_Contents")
      enable("slct_CompareTkrSlct")
      enable("slct_retfreq")
      enable("slct_benchmark_port")
      enable("btn_updateCompareChart")
      enable("btn_PortSpecfill")
      enable("port_sectionTwo_Optim_slctTkrs")
      enable("port_sectionTwo_Optim_daterange")
      runjs(HTML("$('.input-daterange > .form-control').prop('readonly', true)"))
      enable("port_sectionTwo_Optim_retfreq")
      enable("port_sectionTwo_Optim_UseMinMax")
      enable("port_sectionTwo_Optim_slct_Rf")
      enable("port_sectionTwo_OptimBtn")
      runjs(HTML("document.getElementById('port_sectionTwo_OptimBtn').style.backgroundColor = 'tomato'"))

    }
  }
)




source("Server/PortfolioTabServer_SectionOne.R", local = T)$value

source("Server/PortfolionTabServer_SectionTwo.R", local = T)$value

source("Server/PortfolionTabServer_SectionThree.R", local = T)$value