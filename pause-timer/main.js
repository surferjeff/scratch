(function() {
    let mins = Number(prompt("Pause in how many minutes?"));
    let pause = () => {
        /* For SiriusXM */
        let pauseButton = document.querySelector('button[aria-label="Pause"]');
        if (pauseButton) {
            pauseButton.click();
            return;
        }
        /* For AM 1710 */
        let abn = document.getElementById("abnaudio");
        if (abn) {
            abn.pause();
            return;
        }
        throw new Error("Failed to find pause button or audio stream");
    };
    if (mins > 0) {
        let timeout = window.setTimeout(pause, mins * 60 * 1000);
        console.log("Set timeout", timeout);
    }
}())