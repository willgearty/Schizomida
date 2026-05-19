// Custom JS for the Schizomida Trait Database app.
// Loaded as an external script from www/ via tags$script(src = "app.js").

// Dark mode widget (uses darkmode-js loaded from CDN in app.R)
function addDarkmodeWidget() {
  new Darkmode({label: '🌓', left: '32px', right: 'unset'}).showWidget();
}
window.addEventListener('load', addDarkmodeWidget);

// Add "Contribute" link to the navbar once the DOM is ready
$(function() {
  $('.navbar-collapse').append(
    $("<a href='https://github.com/willgearty/Schizomida/issues' target='_blank' class='btn btn-primary'>Contribute</a>")
  );
});

// Handle back/forward browser button navigation: forward parent-frame hash
// changes into the shiny input #currentHash (see navbar observers in app.R)
$(function() {
  $(window.parent).on('hashchange', function (e) {
    $('#currentHash').val(window.parent.location.hash).change();
  });
});
