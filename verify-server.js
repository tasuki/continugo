const net = require("net");
const fs = require("fs");
const readline = require("readline");
const { Elm } = require("./verifier.js");

const app = Elm.MainVerifier.init({});
const socketPath = "/tmp/verifier.sock";

if (fs.existsSync(socketPath)) {
    fs.unlinkSync(socketPath);
}

const server = net.createServer((client) => {
    console.log("Client connected");
    const rl = readline.createInterface({ input: client, output: client });

    rl.on("line", (line) => {
        console.log("Received:", line);
        try {
            const { game, move } = JSON.parse(line);
            app.ports.get.send({ game, move });
        } catch (err) {
            console.error("Invalid JSON:", err);
            client.write(JSON.stringify({ error: "Invalid JSON" }) + "\n");
        }
    });

    app.ports.put.subscribe((response) => {
        client.write(JSON.stringify(response) + "\n");
    });

    client.on("error", (err) => {
        console.error("Socket error:", err);
    });

    client.on("end", () => {
        console.log("Client disconnected")
        rl.close()
    });
});

server.listen(socketPath, () => console.log(`Listening on ${socketPath}`));
