$(document).ready(function () {
  $("button").click(function () {
    const id = this.id;
    const hidden = $(`span#span${id}`).attr("hidden");
    if (hidden) {
      $(this).text("-");
      $(`span#span${id}`).removeAttr("hidden");
    } else {
      $(this).text("+");
      $(`span#span${id}`).attr("hidden", "true");
    }
  })
})