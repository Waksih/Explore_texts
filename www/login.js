$(document).ready(function() {
  // Custom message handler to set the isLoggedIn and showSignup input values
  Shiny.addCustomMessageHandler("setInputs", function(inputs) {
    Shiny.setInputValue("isLoggedIn", inputs.isLoggedIn);
    Shiny.setInputValue("showSignup", inputs.showSignup);
  });
  
  // Custom message handler to set the setIsLoggedIn input value
  Shiny.addCustomMessageHandler("setIsLoggedIn", function(value) {
    Shiny.setInputValue("isLoggedIn", value);
  });

  // Function to handle the signup link click
  window.signupLinkClick = function() {
    Shiny.setInputValue("showSignup", true);
    $("#login").hide();
    $("#signup").show();
  }

  // Function to handle the login link click
  window.loginLinkClick = function() {
    Shiny.setInputValue("showSignup", false);
    $("#signup").hide();
    $("#login").show();
  }
  
 // Custom message handler to show the dashboard content
  Shiny.addCustomMessageHandler("showDashboardContent", function() {
    $("#dashboardContent").show();
  });

});
