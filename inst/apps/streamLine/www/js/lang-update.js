Shiny.addCustomMessageHandler('updateLang', function(message) {
  $('html').attr('lang', message.lang);
});
