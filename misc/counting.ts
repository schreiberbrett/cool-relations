const n = 14;

let riffleSize1: number[] = (
    [...Array(n)].fill(0))

let riffleSize2: number[][] = (
    [...Array(n)].map(_=>
        [...Array(n)].fill(0)))

for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
        if (i == 0) {
            riffleSize2[i][j] = 1;
        } else if (j == 0) {
            riffleSize2[i][j] = 1;
        } else {
            riffleSize2[i][j] = (
                riffleSize2[i - 1][j] +
                riffleSize2[i][j - 1])
        }
    }
}

let riffleSize3: number[][][] = (
    [...Array(n)].map(_=>
        [...Array(n)].map(_ =>
            [...Array(n)].fill(0))))

for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
        for (let k = 0; k < n; k++) {
            if (i == 0) {
                riffleSize3[i][j][k] = riffleSize2[j][k];
            } else if (j == 0) {
                riffleSize3[i][j][k] = riffleSize2[i][k];
            } else if (k == 0) {
                riffleSize3[i][j][k] = riffleSize2[i][j];
            } else {
                riffleSize3[i][j][k] = (
                    riffleSize3[i - 1][j][k] +
                    riffleSize3[i][j - 1][k] +
                    riffleSize3[i][j][k - 1])
            }
        }
    }
}

let riffleSize4: number[][][][] = (
    [...Array(n)].map(_=>
        [...Array(n)].map(_ =>
            [...Array(n)].map(_ =>
                [...Array(n)].fill(0)))))

for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
        for (let k = 0; k < n; k++) {
            for (let l = 0; l < n; l++) {
                if (i == 0) {
                    riffleSize4[i][j][k][l] = riffleSize3[j][k][l];
                } else if (j == 0) {
                    riffleSize4[i][j][k][l] = riffleSize3[i][k][l];
                } else if (k == 0) {
                    riffleSize4[i][j][k][l] = riffleSize3[i][j][l];
                } else if (l == 0) {
                    riffleSize4[i][j][k][l] = riffleSize3[i][j][k];
                } else {
                    riffleSize4[i][j][k][l] = (
                        riffleSize4[i - 1][j][k][l] +
                        riffleSize4[i][j - 1][k][l] +
                        riffleSize4[i][j][k - 1][l] +
                        riffleSize4[i][j][k][l - 1])
                }
            }
        }
    }
}

console.log(riffleSize4[13][4][4][4])