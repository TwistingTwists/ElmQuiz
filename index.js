import { Elm } from "./src/ElmQuiz.elm";

var storedState = localStorage.getItem("main");
var startingState = storedState ? JSON.parse(storedState) : null;
var app = Elm.ElmQuiz.init({ flags: startingState });
app.ports.setStorage.subscribe(function(state) {
  localStorage.setItem("main", JSON.stringify(state));
});



// Elm.ElmQuiz.init({
//   node: document.querySelector("main")
// });
