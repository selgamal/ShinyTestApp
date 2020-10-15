shinyBS.inputBindings.modal.initialize = function(el) {
    $($(el).attr("data-sbs-trigger")).attr({"data-toggle": "modal", "data-target": $(el).attr("id")});
  };


var alert1;
$(document).on("click", "#alert1", function() {
    alert1 = Math.random();
    Shiny.onInputChange("clkd", alert1);
});

/*
$( document ).ready(function() {
      document.querySelector("#main_tabs > li:nth-child(5) > a").href="";
});
*/

$(document).on("click", "#main_tabs > li > a", function() {$(window).trigger('resize')});

$(document).on("click", "#div_price_fs > li", function() {$(window).trigger('resize')});

$(document).on("click", "#slct_Tkr_fs > li", function() {$(window).trigger('resize')});

$( document ).ready(function() {

$(window).resize(function() {
    $("#vbx_spy > a > div").css("height", $("#vbx_sp > a > div").height());
    $("#vbx_nasdaq > a > div").css("height", $("#vbx_sp > a > div").height());
    $("#vbx_tkrgnr > a > div").css("height", $("#vbx_sp > a > div").height());
    $("#vbx_tkrlosr > a > div").css("height", $("#vbx_sp > a > div").height());
   $("#vbx_spy > a > div > div.inner > h3 > span:nth-child(1)").css("width", $("#vbx_sp > a > div > div.inner > h3 > span:nth-child(1)").width());
    $("#vbx_nasdaq > a > div > div.inner > h3 > span:nth-child(1)").css("width", $("#vbx_sp > a > div > div.inner > h3 > span:nth-child(1)").width());
    
        $("#vbx_tkrgnr > a > div > div.inner > h3 > span:nth-child(1)").css("width", $("#vbx_sp > a > div > div.inner > h3 > span:nth-child(1)").width());
        
            $("#vbx_tkrlosr > a > div > div.inner > h3 > span:nth-child(1)").css("width", $("#vbx_sp > a > div > div.inner > h3 > span:nth-child(1)").width());
    $("#MonthlyYC > div > div > div.dataTables_scrollBody").css("height",$("#YC_Charts_Container").height()-$("#MonthlyYC > div > div > div.dataTables_scrollHead").height());
    $("#BSplot1").css("height", $("#BS").height());
    $("#ISplot1").css("height", $("#IS").height());
    $("#CFplot1").css("height", $("#tkr_fs_ratios > div:nth-child(6) > div:nth-child(1)").height()-$("#tkr_fs_ratios > div:nth-child(6) > div:nth-child(2) > h4").height()-20);
    /*disable manual input in date input boxes*/
  $('.input-daterange > .form-control').prop('readonly', true);
  $('.input-daterange > .form-control').css('background-color', "white");
  $('.shiny-date-input > .form-control').prop('readonly', true);

});
});


$(document).on('click', '#showsidebar', function () {
              $('#sidebarpan').show('slide',{direction:'left'},800);
              $('body > div.container-fluid > div.tab-content').animate({'padding-left': '19em'}, 'slow');
              setTimeout(function(){
                    $(window).trigger('resize');
                          }, 850);
              
});


$(document).on('click', '#hidesidebar', function () {
              $('#sidebarpan').hide('slide',{direction:'left'},600);
              $('body > div.container-fluid > div.tab-content').animate({'padding-left': '2em'}, 'slow');
              setTimeout(function(){
                    $(window).trigger('resize');
                          }, 650);
});

/* Clickable boxes return ticker symbol*/

var slctd;

$(document).on("click", ".small-box", function(){ slctd =$(this).find('#bxsym').html();
Shiny.onInputChange("js_symbl_clkd", [slctd,Math.random()]);
});

/* Clickable bubbles return ticker symbol*/


$(document).on("click", "#bublz > svg > g > text", function(){ slctd = this.innerHTML;
Shiny.onInputChange("js_symbl_clkd", [slctd,Math.random()]);
});
$(document).on("click", "#bublz > svg > g > circle", function(){ slctd = this.nextSibling.innerHTML;
Shiny.onInputChange("js_symbl_clkd", [slctd,Math.random()]);
});

$(document).on("mouseover", "#bublz > svg > g > circle", function(){ $(this).css("cursor", "pointer");
});

$(document).on("mouseover", "#bublz > svg > g > text", function(){ $(this).css("cursor", "pointer");
});

$(document).on("mouseover", "#bublz > svg > g:nth-child(1) > circle", function(){ $(this).css("cursor", "");
});

/* Selections in the ticker details tab*/

$(document).on("click", "#lnk", function(){ tk = this.innerHTML;
$("#slct_Tkr_fs").find(".active").removeClass("active");
$(this).parent().addClass("active");
Shiny.onInputChange("js_tkrlnk", [tk,Math.random()]);
});


$(document).on("click", "#price_fs", function(){ tk = this.innerHTML;
$("#div_price_fs").find(".active").removeClass("active");
$(this).parent().addClass("active");
Shiny.onInputChange("js_price_fs", tk);
});

/*
$(document).ready(function() {
  $("#div_fs_ratio > li:nth-child(1)").addClass("active");
});

*/


/*
$(document).ready(function() {
$("#slct_Tkr_fs > li:nth-child(1) > a").trigger("click");
});*/


/* Detect when modals are shown and when closed (to stop refresh while model is open for better experience)*/

$( document ).ready(function() {
  $("#plotmodal").on("show.bs.modal", function () {
    x = new Date().toLocaleString();
    Shiny.onInputChange("plot_modal_open",["open", x]);
  });
});


$( document ).ready(function() {
  $("#Companiestable").on("show.bs.modal", function () {
    x = new Date().toLocaleString();
    Shiny.onInputChange("CompniesTable_modal_open",["open", x]);
  });
});

$( document ).ready(function() {
  $("#plotmodal").on("hide.bs.modal", function () {
    x = new Date().toLocaleString();
    Shiny.onInputChange("plot_modal_close",["close", x]);
  });
});


$( document ).ready(function() {
  $("#Companiestable").on("hide.bs.modal", function () {
    x = new Date().toLocaleString();
    Shiny.onInputChange("CompniesTable_modal_close",["close", x]);
  });
});


$( document ).ready(function() {
  $("#detailstable").on("show.bs.modal", function () {
    x = new Date().toLocaleString();
    Shiny.onInputChange("plot_modal_open",["open", x]);
  });
});

$( document ).ready(function() {
  $("#detailstable").on("hide.bs.modal", function () {
    x = new Date().toLocaleString();
    Shiny.onInputChange("plot_modal_close",["close", x]);
  });
});

/*
$( document ).ready(function() {
  $("#Chart1Opts").on("shown.bs.modal", function () {
    $(".modal-backdrop.fade.in").css("display", "none");
  });
});   

 */

$(document).on('click', '#HideShowCandlStickTable', function () {
        var mh = $("#CandlStickTableRow").css('max-height')
        if (mh == "0px") {
          $('#CandlStickTableRow').css("max-height","141px");
          $("#HideShowCandlStickTable").text("—");
        } else {
          $('#CandlStickTableRow').css("max-height","0px");
          $("#HideShowCandlStickTable").text("+");
        };
});

$(document).on('click', '#HideShowPerformanceDiv', function () {
        var mh = $("#PriceHistoryPerformanceDiv").css('max-height')
        if (mh == "0px") {
          $('#PriceHistoryPerformanceDiv').css("max-height","1750px");
          $("#HideShowPerformanceDiv").text("—"); 
        } else {
          $('#PriceHistoryPerformanceDiv').css("max-height","0px");
          $("#HideShowPerformanceDiv").text("+");
        };
});

$(document).on('click', '#HideShowValRatiosDiv', function () {
        var mh = $("#valRatiosContainerDiv").css('max-height')
        if (mh == "0px") {
          $('#valRatiosContainerDiv').css("max-height","600px");
          $("#HideShowValRatiosDiv").text("—"); 
        } else {
          $('#valRatiosContainerDiv').css("max-height","0px");
          $("#HideShowValRatiosDiv").text("+");
        };
});


$(document).on('click', '#HideShow_port_sectionOne', function () {
        var mh = $("#PortsectionOne_div").css('max-height')
        if (mh == "0px") {
          $('#PortsectionOne_div').css("max-height","1400px");
          $("#HideShow_port_sectionOne").text("—"); 
        } else {
          $('#PortsectionOne_div').css("max-height","0px");
          $("#HideShow_port_sectionOne").text("+");
        };
});

$(document).on('click', '#HideShow_port_sectionTwo', function () {
        var mh = $("#PortsectionTwo_div").css('max-height')
        if (mh == "0px") {
          $('#PortsectionTwo_div').css("max-height","3000px");
          $("#HideShow_port_sectionTwo").text("—"); 
        } else {
          $('#PortsectionTwo_div').css("max-height","0px");
          $("#HideShow_port_sectionTwo").text("+");
        };
});

$(document).on('click', '#HideShow_port_sectionThree', function () {
        var mh = $("#PortsectionThree_div").css('max-height')
        if (mh == "0px") {
          $('#PortsectionThree_div').css("max-height","3000px");
          $("#HideShow_port_sectionThree").text("—"); 
        } else {
          $('#PortsectionThree_div').css("max-height","0px");
          $("#HideShow_port_sectionThree").text("+");
        };
});
 
$('#myTable').on( 'draw.dt', function () {
    alert( 'Table redrawn' );
} );                                   