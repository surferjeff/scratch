(function() {
    let mins = Number(prompt("Pause in how many minutes?"));
    if (mins > 0) {
        let timeout = window.setTimeout(
            () => document.querySelector('button[aria-label="Pause"]').click(),
            mins * 60 * 1000);
        console.log("Set timeout", timeout);
    }
}())