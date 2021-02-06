import { Elm } from "./Main.elm";

if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}

Elm.Main.init({
  node: document.querySelector("#elm"),
  flags: {},
});
