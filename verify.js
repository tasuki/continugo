const { Elm } = require("./verifier.js");
const app = Elm.MainVerifier.init({});

if (process.argv.length != 4) {
    console.error("Requires exactly two arguments: the game record and the next move");
    process.exit(1);
}

app.ports.put.subscribe((data) => {
    console.log(JSON.stringify(data));
    process.exit(0);
});

app.ports.get.send({"game": process.argv[2], "move": process.argv[3]});
