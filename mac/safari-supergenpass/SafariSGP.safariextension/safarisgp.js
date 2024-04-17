(function () {
  function popoverListener() {
    var url = safari.application.activeBrowserWindow.activeTab.url;
    // Disable subdomain removal so the entire host is available in the form
    document.getElementById("Domain").value = gp2_process_uri(url, true);
  }

  safari.application.addEventListener("popover", popoverListener, true);
}())
