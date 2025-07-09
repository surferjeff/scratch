const start = (
    "ABBC" +
    "ABBC" +
    "DEEF" +
    "DGHF" +
    "I..J"
);

function enumerateMoves(board) {
    let empty1 = board.indexOf('.');
    let empty2 = board.indexOf('.', empty1 + 1);
    let result = enumerateMovesFrom(board, empty1);
    result.push(...enumerateMovesFrom(board, empty2));
    return result;
}

function enumerateMovesFrom(board, i) {
    let x = i % 4;
    let result = [];
    if (x > 0 && board[i-1] != '.') {
        result.push(['right', board[i-1]]);
    }
    if (x < 3 && board[i+1] != '.') {
        result.push(['left', board[i+1]]);
    }
    if (i > 3 && board[i - 4] != '.') {
        result.push(['down', board[i - 4]]);
    }
    if (i < 16 && board[i + 4] != '.') {
        result.push(['up', board[i + 4]])
    }
    return result;
}

console.log(enumerateMoves(start));