## これは、単配列である。　##


gen_rand <- function(x, arr_length) {
    arr <- list()
    for (i in 1:arr_length) {
        #333万1分の1の確率で1
        #333万分の1の確率で 2, 3
        if (x[i] >= 0 && x[i] <= 1) {
            arr <- append(arr, 1)
        } else if (x[i] > 1 && x[i] <= 2) {
            arr <- append(arr, 2)
        } else if (x[i] > 2 && x[i] <= 3) {
            arr <- append(arr, 3)
        }
    }
    return(arr)
}

arr_length <- 10
x <- runif(arr_length, min = 0, max = 3)

arr <- gen_rand(x, arr_length)

##################################################

gen_group <- function(x, arr_length) {
    #333万1分の1の確率で1
    #333万分の1の確率で 2, 3, 4, 5, 6, 7, 8, 9, 10
    arr <- list()
    for (i in 1:arr_length) {
        if (x[i] >= 0 && x[i] <= 1) {
            arr <- append(arr, 1)
        } else if (x[i] > 1 && x[i] <= 2) {
            arr <- append(arr, 2)
        } else if (x[i] > 2 && x[i] <= 3) {
            arr <- append(arr, 3)
        } else if (x[i] > 3 && x[i] <= 4) {
            arr <- append(arr, 4)
        } else if (x[i] > 4 && x[i] <= 5) {
            arr <- append(arr, 5)
        } else if (x[i] > 5 && x[i] <= 6) {
            arr <- append(arr, 6)
        } else if (x[i] > 6 && x[i] <= 7) {
            arr <- append(arr, 7)
        } else if (x[i] > 7 && x[i] <= 8) {
            arr <- append(arr, 8)
        } else if (x[i] > 8 && x[i] <= 9) {
            arr <- append(arr, 9)
        } else if (x[i] > 9 && x[i] <= 10) {
            arr <- append(arr, 10)
        }
    }
    return(arr)
}

arr_length <- 100
y <- runif(arr_length, min = 0, max = 10)
gen_group(y, arr_length)
