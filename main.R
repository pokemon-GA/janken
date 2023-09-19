#初期集団の生成
#生成関数
gen_group <- function(x) {
    #1000万分の100万1の確率で1
    #1000万分の100万の確率で 2, 3, 4, 5, 6, 7, 8, 9, 10
    arr <- list()
    arr_length <- length(x)
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

#実際の生成
#戦略の集団サイズ
group_size <- 100
#戦略サイズ
storategy_pattern <- 10

gen_initialize_rand_group <- runif(group_size, min = 0, max = storategy_pattern)
initialize_group <- gen_group(gen_initialize_rand_group)


######ここまでエラーなし######

#評価関数前の各戦略の具体的なデータの代入
input_data <- function(group) { # nolint
    #具体的なデータの代入
    result_array <- list()
    #戦略
    #書く戦略は、10回分の勝負を1つの配列に入れている
    # 1 ... グー
    # 2 ... チョキ
    # 3 ... パー
    #戦略パターンを1~10としている
    #パターン1
    #全てグー
    pattern_1 <- function() {
        array <- list(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        return(array)
    }
    #パターン2
    #全てチョキ
    pattern_2 <- function() {
        array <- list(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
        return(array)
    }
    #パターン3
    #全てパー
    pattern_3 <- function() {
        array <- list(3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
        return(array)
    }
    #パターン4
    #グー始まりのローテーション
    pattern_4 <- function() {
        array <- list(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
        return(array)
    }
    #パターン5
    #チョキ始まりのローテーション
    pattern_5 <- function() {
        array <- list(2, 3, 1, 2, 3, 1, 2, 3, 1, 2)
        return(array)
    }
    #パターン6
    #パー始まりのローテーション
    pattern_6 <- function() {
        array <- list(3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
        return(array)
    }
    #パターン7
    #ランダム (疑似一様乱数)
    pattern_7 <- function() { # nolint
        gen_rand <- function(x, array_length) {
            array <- list()
            for (i in 1:array_length) {
                #333万1分の1の確率で1
                #333万分の1の確率で2
                #333万1分の1の確率で3
                if (x[i] >= 0 && x[i] <= 1) {
                    array <- append(array, 1)
                } else if (x[i] > 1 && x[i] <= 2) {
                    array <- append(array, 2)
                } else if (x[i] > 2 && x[i] <= 3) {
                    array <- append(array, 3)
                }
            }
            return(array)
        }
        array_length <- 10
        x <- runif(array_length, min = 0, max = 3)
        array <- gen_rand(x, array_length)
        return(array)
    }
    #パターン8
    #グー始まりのローテーション (2回ずつ)
    pattern_8 <- function() {
        array <- list(1, 1, 2, 2, 3, 3, 1, 1, 2, 2)
        return(array)
    }
    #パターン9
    #チョキ始まりのローテーション (2回ずつ)
    pattern_9 <- function() {
        array <- list(2, 2, 3, 3, 1, 1, 2, 2, 3, 3)
        return(array)
    }
    #パターン10
    #パー始まりのローテーション (2回ずつ)
    pattern_10 <- function() {
        array <- list(3, 3, 1, 1, 2, 2, 3, 3, 1, 1)
        return(array)
    }
    array_length <- length(group)
    for (i in 1:array_length) {
        if (group[i] == 1) {
            storategy <- pattern_1()
            result_array <- append(result_array, storategy)
        } else if (group[i] == 2) {
            storategy <- pattern_2()
            result_array <- append(result_array, storategy)
        } else if (group[i] == 3) {
            storategy <- pattern_3()
            result_array <- append(result_array, storategy)
        } else if (group[i] == 4) {
            storategy <- pattern_4()
            result_array <- append(result_array, storategy)
        } else if (group[i] == 5) {
            storategy <- pattern_5()
            result_array <- append(result_array, storategy)
        } else if (group[i] == 6) {
            storategy <- pattern_6()
            result_array <- append(result_array, storategy)
        } else if (group[i] == 7) {
            storategy <- pattern_7()
            result_array <- append(result_array, storategy)
        } else if (group[i] == 8) {
            storategy <- pattern_8()
            result_array <- append(result_array, storategy)
        } else if (group[i] == 9) {
            storategy <- pattern_9()
            result_array <- append(result_array, storategy)
        } else if (group[i] == 10) {
            storategy <- pattern_10()
            result_array <- append(result_array, storategy)
        }
    }
    return(result_array)
}

molded_group <- input_data(initialize_group)



#https://cell-innovation.nig.ac.jp/SurfWiki/vector_difference.html