function gameId() {
  var parts = window.location.href.split("/");
  return parts[parts.length - 1];
};
