const net = require("net");
const readline = require("readline");

function sendRequest(game, move) {
    return new Promise((resolve, reject) => {
        const client = net.createConnection("/tmp/verifier.sock", () => {
            console.log("Connected, sending data...");
            client.write(JSON.stringify({ game, move }) + "\n");
        });

        const rl = readline.createInterface({ input: client });

        rl.on("line", (line) => {
            resolve(JSON.parse(line));
            rl.close();
            client.end();
        });

        client.on("error", reject);
    });
}

async function main() {
    for (i = 0; i < 10; i++) {
        console.log(await sendRequest("B[fifi];W[msej];B[eohb];W[fNcm];B[kVdW];W[dvdI];B[mEcL];W[obcY];B[onbA];W[pKbN];B[mefG];W[nBfT];B[nnhq];W[oKhD];B[owja];W[pTjn];B[pFkK];W[rckX];B[qOmu];W[slmH];B[rXoe];W[fbol];B[gdpn];W[heok];B[eapo];W[hhqn];B[dXnl];W[eYmi];B[dUli];W[eVkf];B[dPjf];W[fbip];B[crhI]", "W[llph]"));
        console.log(await sendRequest("B[fifi];W[msej];B[eohb];W[fNcm];B[kVdW];W[dvdI];B[mEcL];W[obcY];B[onbA];W[pKbN];B[mefG];W[nBfT];B[nnhq];W[oKhD];B[owja];W[pTjn];B[pFkK];W[rckX];B[qOmu];W[slmH];B[rXoe];W[fbol];B[gdpn];W[heok];B[eapo];W[hhqn];B[dXnl];W[eYmi];B[dUli];W[eVkf];B[dPjf];W[fbip];B[crhI]", "W[laph]"));
        console.log(await sendRequest("B[fifi];W[msej];B[eohb];W[fNcm];B[kVdW];W[dvdI];B[mEcL];W[obcY];B[onbA];W[pKbN];B[mefG];W[nBfT];B[nnhq];W[oKhD];B[owja];W[pTjn];B[pFkK];W[rckX];B[qOmu];W[slmH];B[rXoe];W[fbol];B[gdpn];W[heok];B[eapo];W[hhqn];B[dXnl];W[eYmi];B[dUli];W[eVkf];B[dPjf];W[fbip];B[crhI]", "W[gaph]"));
    }
}

main();
