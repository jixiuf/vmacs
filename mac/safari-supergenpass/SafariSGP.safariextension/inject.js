(function () {
  function completePassword(message) {
    if (message.name !== "completePassword") {
      return;
    }
    var password = message.message;
    console.log("completePassword(" + password + ")");
    var inputElements = document.getElementsByTagName("input");
    for (var i = 0; i < inputElements.length; i++) {
      var inputElement = inputElements[i];
      if (inputElement.type === "password" && inputElement.value === "") {
        inputElement.value = password;
      }
    }
  }

  safari.self.addEventListener("message", completePassword, false);
} ())