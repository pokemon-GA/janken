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















#########################
#戦略関数
#以下を引数に取る
the_number_of_times <- 10

#パターン7
######ランダム (疑似一様乱数)
strategy_uniform_random <- function(the_number_of_times) { # nolint
    gen_rand <- function(x, the_number_of_times) {
        strategy_data_frame <- data.frame()
        for (i in 1:the_number_of_times) {
            #333万1分の1の確率で1
            #333万分の1の確率で2
            #333万1分の1の確率で3
            if (x[i] >= 0 && x[i] <= 1) {
                strategy_data_frame <- rbind(strategy_data_frame, 1)
            } else if (x[i] > 1 && x[i] <= 2) {
                strategy_data_frame <- rbind(strategy_data_frame, 2)
            } else if (x[i] > 2 && x[i] <= 3) {
                strategy_data_frame <- rbind(strategy_data_frame, 3)
            }
        }
        return(strategy_data_frame)
    }
    x <- runif(the_number_of_times, min = 0, max = 3)
    strategy_data_frame <- gen_rand(x, the_number_of_times)
    return(strategy_data_frame)
}

#実行関数
the_number_of_times <- 10
strategy_uniform_random(the_number_of_times)




######全て同じ
strategy_all_the_same <- function(the_number_of_times, number) {
    strategy_data_frame <- data.frame()
    for (i in 1:the_number_of_times) {
        strategy_data_frame <- rbind(strategy_data_frame, number)
    }
    return(strategy_data_frame)
}

#実行関数
the_number_of_times <- 10
number <- 1
strategy_all_the_same(the_number_of_times, number)



####俺の思考
# cycle = 1
# 三のあまり
# ※1始まり
# 1
# 2
# 0

# cycle = 2
# 六のあまり
# ※1始まり
# 1,2
# 3,4
# 5,0

# syscle = 3
# 九のあまり
# ※1始まり
# 1,2,3
# 4,5,6
# 7,8,9

# if ~
#     cycle分forを回す

#first_number
# 1の時
# second_number = first_number + 1
# second_number = first_number + 2

######サイクル
#仮パラメーターの説明
#the_number_of_times <-略
#cycle <-同じ手を何回出してサイクルするか
#first_number <-どの手を最初に出すか
####以上！
#cycleは、必ずthe_number_of_timeより小さくなければならない
#オーバーしている場合は、全てfirst_numberを出す
#first_numberは、1,2,3のどれかでなければいけない
#異なる場合は、エラー
#戦略
#書く戦略は、10回分の勝負を1つの配列に入れている
# 1 ... グー
# 2 ... チョキ
# 3 ... パー
#戦略パターンを1~10としている
strategy_cycle <- function(the_number_of_times, cycle, first_number) { # nolint
    strategy_data_frame <- data.frame()
    #first_numberの処理
    if (first_number == 1) {
        second_number <- first_number + 1
        third_number <- first_number + 2
    } else if (first_number == 2) {
        second_number <- first_number + 1
        third_number <- first_number - 1
    } else if (first_number == 3) {
        second_number <- first_number - 2
        third_number <- first_number - 1
    } else {
        #じゃんけんなのに、1,2,3意外入れるな！
        stop('first_number has only 1, 2 or 3.')
    }
    if (the_number_of_times < cycle) {
        #サイクルサイズが戦略サイズよりでかいとは何事か
        #まあ、いいだろう
        #全て同じfirst_numberの手にする
        strategy_data_frame <- strategy_all_the_same(the_number_of_times, first_number) # nolint
        #ここまでOK!
    } else {
        #余りを出すための配列を吐く関数
        surplus <- function(the_number_of_times) {
            surplus_array <- c()
            for (i in 1:the_number_of_times) {
                surplus_array <- append(surplus_array, i)
            }
            return(surplus_array)
        }
        surplus_array <- surplus(the_number_of_times)
        for (i in 1:the_number_of_times) {
            if (nrow(strategy_data_frame) < the_number_of_times) {
                #処理書く部分
                if (surplus_array[i] %% 3 == 1) {
                    for (i in 1:cycle) {
                        strategy_data_frame <- rbind(strategy_data_frame, first_number) # nolint
                        #目的の戦略回数になったら、抜ける
                        if (nrow(strategy_data_frame) >= the_number_of_times) {
                            break
                        }
                    }
                } else if (surplus_array[i] %% 3 == 2) {
                    for (i in 1:cycle) {
                        strategy_data_frame <- rbind(strategy_data_frame, second_number) # nolint
                        #目的の戦略回数になったら、抜ける
                        if (nrow(strategy_data_frame) >= the_number_of_times) {
                            break
                        }
                    }
                } else if (surplus_array[i] %% 3 == 0) {
                    for (i in 1:cycle) {
                        strategy_data_frame <- rbind(strategy_data_frame, third_number) # nolint
                        #目的の戦略回数になったら、抜ける
                        if (nrow(strategy_data_frame) >= the_number_of_times) {
                            break
                        }
                    }
                }
                #目的の戦略回数になったら、抜ける
                if (nrow(strategy_data_frame) >= the_number_of_times) {
                    break
                }
            } else {
                break
            }
        }
    }
    return(strategy_data_frame)
}

#実行関数
the_number_of_times <- 10
cycle <- 1
first_number <- 1
strategy_cycle(the_number_of_times, cycle, first_number)

the_number_of_times <- 10
cycle <- 20
first_number <- 1
strategy_cycle(the_number_of_times, cycle, first_number)



the_number_of_times <- 10
cycle <- 3
first_number <- 2
strategy_cycle(the_number_of_times, cycle, first_number)

the_number_of_times <- 10
cycle <- 5
first_number <- 3
strategy_cycle(the_number_of_times, cycle, first_number)

#エラーパターン
the_number_of_times <- 10
cycle <- 5
first_number <- 5
strategy_cycle(the_number_of_times, cycle, first_number)



###########################以下、実験場####################################
#別実験
# x <- data.frame()
# for (i in 1:10) {
#     if(nrow(x) < 10) {
#         for (j in 1:11) {
#             x <- rbind(x, j)
#         }
#     } else {
#         break
#     }
# }
#結果
#    X1L
# 1    1
# 2    2
# 3    3
# 4    4
# 5    5
# 6    6
# 7    7
# 8    8
# 9    9
# 10  10
# 11  11

#本実験
# strategy_cycle <- function(the_number_of_times, cycle, first_number) { # nolint
#     strategy_data_frame <- data.frame()
#     #first_numberの処理
#     if (first_number == 1) {
#         second_number <- first_number + 1
#         third_number <- first_number + 2
#     } else if (first_number == 2) {
#         second_number <- first_number + 1
#         third_number <- first_number - 1
#     } else if (first_number == 3) {
#         second_number <- first_number - 2
#         third_number <- first_number - 1
#     } else {
#         #じゃんけんなのに、1,2,3意外入れるな！
#         print('error')
#     }
#     strategy_data_frame <- data.frame(first_number, second_number, third_number)
#     return(strategy_data_frame)
# }

# the_number_of_times <- 10
# cycle <- 1
# first_number <- 1
# strategy_cycle(the_number_of_times, cycle, first_number)


# surplus <- function(the_number_of_times) {
#     surplus_array <- c()
#     for (i in 1:the_number_of_times) {
#         surplus_array <- append(surplus_array, i)
#     }
#     return(surplus_array)
# }
# surplus_array <- surplus(the_number_of_times)








#内部に関数を埋め込める
# a <- function() {
#     return(1)
# }

# b <- function(a) {
#     c <- a()
#     ans <- c + 1
#     return(ans)
# }

# result <- b(a)






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
        array <- data.frame(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        return(array)
    }
    #パターン2
    #全てチョキ
    pattern_2 <- function() {
        array <- data.frame(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
        return(array)
    }
    #パターン3
    #全てパー
    pattern_3 <- function() {
        array <- data.frame(3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
        return(array)
    }
    #パターン4
    #グー始まりのローテーション
    pattern_4 <- function() {
        array <- data.frame(1, 2, 3, 1, 2, 3, 1, 2, 3, 1)
        return(array)
    }
    #パターン5
    #チョキ始まりのローテーション
    pattern_5 <- function() {
        array <- data.frame(2, 3, 1, 2, 3, 1, 2, 3, 1, 2)
        return(array)
    }
    #パターン6
    #パー始まりのローテーション
    pattern_6 <- function() {
        array <- data.frame(3, 1, 2, 3, 1, 2, 3, 1, 2, 3)
        return(array)
    }
    #パターン7
    #ランダム (疑似一様乱数)
    pattern_7 <- function() { # nolint
        gen_rand <- function(x, array_length) {
            array <- data.frame()
            for (i in 1:array_length) {
                #333万1分の1の確率で1
                #333万分の1の確率で2
                #333万1分の1の確率で3
                if (x[i] >= 0 && x[i] <= 1) {
                    array <- rbind(array, 1)
                } else if (x[i] > 1 && x[i] <= 2) {
                    array <- rbind(array, 2)
                } else if (x[i] > 2 && x[i] <= 3) {
                    array <- rbind(array, 3)
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
        # array <- data.frame(1, 1, 2, 2, 3, 3, 1, 1, 2, 2)
        array <- data.frame()
        array <- rbind(array, 1)
        array <- rbind(array, 1)
        array <- rbind(array, 2)
        array <- rbind(array, 2)
        array <- rbind(array, 3)
        array <- rbind(array, 3)
        array <- rbind(array, 1)
        array <- rbind(array, 1)
        array <- rbind(array, 2)
        array <- rbind(array, 2)
        return(array)
    }
    #パターン9
    #チョキ始まりのローテーション (2回ずつ)
    pattern_9 <- function() {
        array <- data.frame(2, 2, 3, 3, 1, 1, 2, 2, 3, 3)
        return(array)
    }
    #パターン10
    #パー始まりのローテーション (2回ずつ)
    pattern_10 <- function() {
        array <- data.frame(3, 3, 1, 1, 2, 2, 3, 3, 1, 1)
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









# input_data <- function(...) {
#     a <- list(...)
#     return(a)
# }











input_data <- function(
        group,
        ...
    ) { # nolint
    #具体的なデータの代入
    result_array <- list()
    array_length <- length(group)
    #patternの配列の生成
    pattern <- list(...)
    for (i in 1:array_length) {
        strategy_number <- as.integer(group[i])
        # colnames(pattern[strategy_number]) <- c(i)
        result_array <- append(result_array, pattern[strategy_number])
    }
    return(result_array)
}



ex <- function(
    group,
    ...
    ) {
    result_array <- list()
    array_length <- length(group)
    pattern <- list(...)
    for (i in 1:array_length) {
        strategy_number <- as.integer(group[i])
        result_array <- append(result_array, pattern[strategy_number])
    }
    return(result_array)
}


abc <- ex(
    group = initialize_group,
    pattern_1,
    pattern_2,
    pattern_3,
    pattern_4,
    pattern_5,
    pattern_6,
    pattern_7,
    pattern_8,
    pattern_9,
    pattern_10
)










































##########################一般化した評価関数##########################
######ランダム (疑似一様乱数)
strategy_uniform_random <- function(the_number_of_times) { # nolint
    strategy_data_frame <- data.frame()
    random <- sample(1:3, the_number_of_times, replace = TRUE, prob = NULL)
    for (i in 1:the_number_of_times) {
        strategy_data_frame <- rbind(strategy_data_frame, random[i])
    }
    colnames(strategy_data_frame) <- c('strategy')
    return(strategy_data_frame)
}

######全て同じ
strategy_all_the_same <- function(the_number_of_times, number) {
    strategy_data_frame <- data.frame()
    #numberのエラーハンドリング
    if (number != 1 && number != 2 && number != 3) {
        #じゃんけんなのに、1,2,3意外入れるな！
        stop('first_number has only 1, 2 or 3.')
    }
    for (i in 1:the_number_of_times) {
        strategy_data_frame <- rbind(strategy_data_frame, number)
    }
    colnames(strategy_data_frame) <- c('strategy')
    return(strategy_data_frame)
}

######サイクル
strategy_cycle <- function(the_number_of_times, cycle, first_number) { # nolint
    strategy_data_frame <- data.frame()
    #first_numberの処理
    if (first_number == 1) {
        second_number <- first_number + 1
        third_number <- first_number + 2
    } else if (first_number == 2) {
        second_number <- first_number + 1
        third_number <- first_number - 1
    } else if (first_number == 3) {
        second_number <- first_number - 2
        third_number <- first_number - 1
    } else {
        #じゃんけんなのに、1,2,3意外入れるな！
        stop('first_number has only 1, 2 or 3.')
    }
    if (the_number_of_times < cycle) {
        #サイクルサイズが戦略サイズよりでかいとは何事か
        #まあ、いいだろう
        #全て同じfirst_numberの手にする
        strategy_data_frame <- strategy_all_the_same(the_number_of_times, first_number) # nolint
        #ここまでOK!
    } else if (the_number_of_times <= 0) {
        #じゃんけんなのに、1,2,3意外入れるな！
        stop('the_number_of_times has to input more than 0.')
    } else {
        #余りを出すための配列を吐く関数
        surplus <- function(the_number_of_times) {
            surplus_array <- c()
            for (i in 1:the_number_of_times) {
                surplus_array <- append(surplus_array, i)
            }
            return(surplus_array)
        }
        surplus_array <- surplus(the_number_of_times)
        for (i in 1:the_number_of_times) {
            if (nrow(strategy_data_frame) < the_number_of_times) {
                #処理書く部分
                if (surplus_array[i] %% 3 == 1) {
                    for (i in 1:cycle) {
                        strategy_data_frame <- rbind(strategy_data_frame, first_number) # nolint
                        #目的の戦略回数になったら、抜ける
                        if (nrow(strategy_data_frame) >= the_number_of_times) {
                            break
                        }
                    }
                } else if (surplus_array[i] %% 3 == 2) {
                    for (i in 1:cycle) {
                        strategy_data_frame <- rbind(strategy_data_frame, second_number) # nolint
                        #目的の戦略回数になったら、抜ける
                        if (nrow(strategy_data_frame) >= the_number_of_times) {
                            break
                        }
                    }
                } else if (surplus_array[i] %% 3 == 0) {
                    for (i in 1:cycle) {
                        strategy_data_frame <- rbind(strategy_data_frame, third_number) # nolint
                        #目的の戦略回数になったら、抜ける
                        if (nrow(strategy_data_frame) >= the_number_of_times) {
                            break
                        }
                    }
                }
                #目的の戦略回数になったら、抜ける
                if (nrow(strategy_data_frame) >= the_number_of_times) {
                    break
                }
            } else {
                break
            }
        }
    }
    colnames(strategy_data_frame) <- c('strategy')
    return(strategy_data_frame)
}









##################################################################
##################決めることのできるパラメーター集###################
#戦略の集団サイズ
group_size <- 100
#各戦略の長さ
the_number_of_times <- 10
#戦略パターン
strategy_pattern <- 10

#具体的な評価関数
#pattern_1
#ランダム生成
pattern_1 <- strategy_uniform_random(the_number_of_times = the_number_of_times) #nolint
#pattern_2
#全てグー
pattern_2 <- strategy_all_the_same(the_number_of_times = the_number_of_times, number = 1) #nolint
#pattern_3
#全てチョキ
pattern_3 <- strategy_all_the_same(the_number_of_times = the_number_of_times, number = 2) #nolint
#pattern_4
#全てパー
pattern_4 <- strategy_all_the_same(the_number_of_times = the_number_of_times, number = 3) #nolint
#pattern_5
#cycle = 1
#グーはじめ
pattern_5 <- strategy_cycle(the_number_of_times = the_number_of_times, cycle = 1, first_number = 1) #nolint
#pattern_6
#cycle = 1
#チョキはじめ
pattern_6 <- strategy_cycle(the_number_of_times = the_number_of_times, cycle = 1, first_number = 2) #nolint
#pattern_7
#cycle = 1
#パーはじめ
pattern_7 <- strategy_cycle(the_number_of_times = the_number_of_times, cycle = 1, first_number = 3) #nolint
#pattern_8
#cycle = 2
#グーはじめ
pattern_8 <- strategy_cycle(the_number_of_times = the_number_of_times, cycle = 2, first_number = 1) #nolint
#pattern_9
#cycle = 2
#チョキはじめ
pattern_9 <- strategy_cycle(the_number_of_times = the_number_of_times, cycle = 2, first_number = 2) #nolint
#pattern_10
#cycle = 2
#パーはじめ
pattern_10 <- strategy_cycle(the_number_of_times = the_number_of_times, cycle = 2, first_number = 3) #nolint

#初期集団の生成
#生成関数
gen_group <- function(group_size, strategy_pattern) {
    #1000万分の100万1の確率でthe_number_of_value
    #1000万分の100万の確率で 1 ~ (the_number_of_value - 1)
    arr <- list()
    gen_initialize_rand_group <- runif(group_size, min = 0, max = strategy_pattern) #nolint
    for (i in 1:group_size) {
        if (gen_initialize_rand_group[i] == strategy_pattern) {
            arr <- append(arr, strategy_pattern)
        } else {
            input_value <- trunc(gen_initialize_rand_group[i] + 1)
            arr <- append(arr, input_value)
        }
    }
    return(arr)
}
initialize_group <- gen_group(group_size = group_size, strategy_pattern = strategy_pattern) #nolint




unko <- function(group_size, strategy_group) {
    row_label <- 1:group_size
    point <- rep(0, group_size)
    individual_number <- row_label
    gen_strategy_number <- function(group_size, strategy_group) {
        strategy_number_vector <- c()
        for (i in 1:group_size) {
            strategy_number <- strategy_group[[i]]
            strategy_number_vector <- append(strategy_number_vector, strategy_number)
        }
        return(strategy_number_vector)
    }
    strategy_number <- gen_strategy_number(group_size = group_size, strategy_group = strategy_group)
    #!point_list -> point_data_frame
    point_data_frame <- data.frame(
        row.names=row_label,
        point = point,
        strategy_number = strategy_number,
        individual_number = individual_number
    )
    return(point_data_frame)
}

abc <- unko(group_size = group_size, strategy_group = initialize_group)








unkochan <- sample(1:3, 10, replace = TRUE, prob = c(1, 10, 100))


a <- c()
for (i in 1:10) {
    a <- append(a, i)
}

unkochan <- sample(1:10, 5, replace = FALSE, prob = a)





































#dataはｍ流用
#dataのpointをいじりたい
data <- data.frame(
    row.names = sample(1:40, 40, replace = FALSE, prob = NULL),
    point = c(
        100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
        90, 90, 90, 90, 90, 90, 90, 90, 90, 90,
        80, 80, 80, 80, 80, 80, 80, 80, 80, 80,
        70, 70, 70, 70, 70, 70, 70, 70, 70, 70),
    strategy_number = c(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4
    )
)

#####################################################################################################
#ルーレット選択
#group_size
group_size <- 40
#上位x%残す　(0 < ratio < 1)
ratio <- 0.5
#strategy_pattern
strategy_pattern <- 4

roulette_select <- function(data, group_size, ratio) {
    #rationのエラーハンドリング
    #!0 < ratio < 1
    if (0 >= ratio || 1 <= ratio) {
        stop('ratio can only input 0 < ratio < 1.')
    }
    #IEC 60559規格の四捨五入で、整数値にする
    survivor_number <- round(group_size * ratio)
    #エラーハンドリング
    #四捨五入でsurvor == 0 || 1 の時
    if (survivor_number == 0) {
        survivor_number <- 1
        warning('The ratio`s number is 0. You should input ratio larger than your input data.') #nolint
    } else if (survivor_number == group_size) {
        survivor_number <- group_size - 1
        warning('The ratio`s number is 1. You should input ratio less than your input data.') #nolint
    }
    #*ポイントの？をつくろう！
    point_as_vector <- data$point
    select_data_number <- sample(1:group_size, survivor_number, replace = FALSE, prob = point_as_vector) #nolint
    print(select_data_number)
    #*data.frame化
    select_data <- data.frame()
    for (i in 1:survivor_number) {
        each_select_data_number <- select_data_number[i]
        select_data <- rbind(select_data, data[each_select_data_number, ])
    }
    return(select_data)
}


#実行
roulette_select_data <- roulette_select(
    data = data,
    group_size = group_size,
    ratio = ratio
)



#戦略のパターンの割合を計算
#状況をグラフ出力するのに用いる
each_strategy_ratio <- function(select_data, strategy_pattern) {
    sorted_strategy_data <- select_data[order(select_data$strategy_number, decreasing=F),]
    each_strategy_data_items_vector <- c()
    for (i in 1:strategy_pattern) {
        #Headerがstrategy_numberのところで、strategy_numberがi
        filter <- sorted_strategy_data[sorted_strategy_data$strategy_number == i,]
        #filterの行数を調べればいい
        each_strategy_data_items <- nrow(filter)
        each_strategy_data_items_vector <- append(each_strategy_data_items_vector, each_strategy_data_items)
    }
    return(each_strategy_data_items_vector)
}


#実行
roulette_sorted_strategy_data <- each_strategy_ratio(
    select_data = roulette_select_data,
    strategy_pattern = strategy_pattern
)
































