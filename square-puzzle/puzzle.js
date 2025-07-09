

const start = (
    "ABBC" +
    "ABBC" +
    "DEEF" +
    "DGHF" +
    "I..J"
);

const oneTall = 'EGHIJ';
const twoTall = 'ABCDF';
const oneWide = 'ACDFGHIJ';
const twoWide = 'BE';

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
    if (x > 0 && canMove(board, board[i-1], 1)) {
        result.push(['right', board[i-1]]);
    }
    if (x < 3 && canMove(board, board[i+1], -1)) {
        result.push(['left', board[i+1]]);
    }
    if (canMove(board, board[i - 4], 4)) {
        result.push(['down', board[i - 4]]);
    }
    if (canMove(board, board[i + 4] -4)) {
        result.push(['up', board[i + 4]])
    }
    return result;
}

function canMove(board, letter, step) {
    if (!letter) return false;
    for (let i = board.indexOf(letter); i >= 0; i = board.indexOf(letter, i+1)) {
        let adjacent = board[i + step];
        if (adjacent === letter || adjacent === '.') continue;
        return false;
    }
    return true;
}

console.log(enumerateMoves(start));