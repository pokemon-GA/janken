##########################一般化した戦略関数##########################
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
#!group_size >= 10
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

#####################################################################




#初期集団の生成
#生成関数
gen_group <- function(group_size, strategy_pattern) {
    random_vector <- c()
    random <- sample(1:strategy_pattern, group_size, replace = TRUE, prob = NULL)
    for (i in 1:group_size) {
        random_vector <- append(random_vector, random[i])
    }
    return(random_vector)
}
initialize_group <- gen_group(group_size = group_size, strategy_pattern = strategy_pattern) #nolint




#評価関数前の各戦略の具体的なデータの代入
input_data <- function(
        group,
        ...
    ) { # nolint
    #具体的なデータの代入
    result_list <- list()
    list_length <- length(group)
    #patternの配列の生成
    pattern <- list(...)
    for (i in 1:list_length) {
        strategy_number <- as.integer(group[i])
        result_list <- append(result_list, pattern[strategy_number])
    }
    return(result_list)
}

############母集団############
general_population <- input_data(
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

###########バトルシミュレーター#############
#勝ち ... 2点
#あいこ ... 1点
#負け ... 0点
#jankenをやるときの強さの大小
# 1 > 2
# 1 < 3
# 2 > 3
win_point <- 10
lose_point <- 0
draw_point <- 1

battle <- function(group, group_size, the_number_of_times, strategy_group, win_point, lose_point, draw_point) { # nolint
    #点数格納のためのdata.frameを作る
    #*個体番号兼ラベル
    row_label <- 1:group_size
    #*その個体のポイント
    point <- rep(0, group_size)
    #*個体番号
    individual_number <- row_label
    #作戦番号のベクトル生成関数
    gen_strategy_number <- function(group_size, strategy_group) {
        strategy_number_vector <- c()
        for (i in 1:group_size) {
            strategy_number <- strategy_group[[i]]
            strategy_number_vector <- append(strategy_number_vector, strategy_number)
        }
        return(strategy_number_vector)
    }
    #*作戦番号
    strategy_number <- gen_strategy_number(group_size = group_size, strategy_group = strategy_group)
    #!point_list -> point_data_frame
    #?実際の初期化したデータを渡す関数をpoint_listに入れる
    point_data_frame <- data.frame(
        row.names = row_label,
        point = point,
        strategy_number = strategy_number,
        individual_number = individual_number
    )
    #本編の処理
    group_size_loop <- group_size - 1
    for (me in 1:group_size_loop) {
        me_point <- 0
        me_data_frame <- group[[me]]
        me_number <- me + 1
        for (enemy in me_number:group_size) {
            #group[i] vs group[i + 1 ~]
            enemy_point <- 0
            enemy_data_frame <- group[[enemy]]
            for (round_number in 1:the_number_of_times) {
                #あいこ
                if (me_data_frame[round_number, ] == enemy_data_frame[round_number, ]) {
                    me_point <- me_point + draw_point
                    enemy_point <- enemy_point + draw_point
                    #以降、勝ち負けは確定する
                } else if (me_data_frame[round_number, ] == 1 && enemy_data_frame[round_number, ] == 2) {
                    me_point <- me_point + win_point
                    enemy_point <- enemy_point + lose_point
                } else if (me_data_frame[round_number, ] == 1 && enemy_data_frame[round_number, ] == 3) {
                    me_point <- me_point + lose_point
                    enemy_point <- enemy_point + win_point
                } else if (me_data_frame[round_number, ] == 2 && enemy_data_frame[round_number, ] == 1) {
                    me_point <- me_point + lose_point
                    enemy_point <- enemy_point + win_point
                } else if (me_data_frame[round_number, ] == 2 && enemy_data_frame[round_number, ] == 3) {
                    me_point <- me_point + win_point
                    enemy_point <- enemy_point + lose_point
                } else if (me_data_frame[round_number, ] == 3 && enemy_data_frame[round_number, ] == 1) {
                    me_point <- me_point + win_point
                    enemy_point <- enemy_point + lose_point
                } else if (me_data_frame[round_number, ] == 3 && enemy_data_frame[round_number, ] == 2) {
                    me_point <- me_point + lose_point
                    enemy_point <- enemy_point + win_point
                } else {
                    stop('group element can only contain 1, 2 or 3.')
                }
            }
            #各enemyに点数を入れる
            #point_data_frameの得点保存の要素とenemyの番号を照合して、一致したら
            point_data_frame[enemy, 1] <- point_data_frame[enemy, 1] + enemy_point
        }
        #各enemyに点数を入れる
        #point_data_frameの得点保存の要素とenemyの番号を照合して、一致したら
        point_data_frame[me, 1] <- point_data_frame[me, 1] + me_point
    }
    #昇順にソートする
    point_data_frame_ordered <- point_data_frame[order(point_data_frame$point, decreasing=T),]
    #!以下、実験
    #最低点の取得
    min_point <- point_data_frame_ordered[group_size, 1]
    min_point <- round(min_point * 0.999999)
    #TODO: ごり押しで差を開けた
    for (i in 1:group_size) {
        point_data_frame_ordered[i, 1] <- point_data_frame_ordered[i, 1] - min_point #nolint
    }
    return(point_data_frame_ordered)
}

#昇順にソートされたバトル結果のデータフレームを吐き出す関数の実行
data <- battle(
    group = general_population,
    group_size = group_size,
    the_number_of_times = the_number_of_times,
    strategy_group = initialize_group,
    win_point = win_point,
    lose_point = lose_point,
    draw_point = draw_point
)

#####################################################################################################
#ランキング選択
#上位x%残す　(0 < ratio < 1)
ratio <- 0.5

#選択
ranking_select <- function(data, group_size, ratio) {
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
        warning('The ratio`s number is 0. You should input ratio larger than your input data.')
    } else if (survivor_number == group_size) {
        survivor_number <- group_size - 1
        warning('The ratio`s number is 1. You should input ratio less than your input data.')
    }
    select_data <- data[1:survivor_number,]
    return(select_data)
}

#実行
ranking_select_data <- ranking_select(
    data = data,
    group_size = group_size,
    ratio = ratio
)


#####################################################################################################
#ルーレット選択
#上位x%残す　(0 < ratio < 1)
ratio <- 0.8

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




###########################################################################################
#消えた分の生成
#ランダムに生成
gen_new_generation_random <- function(select_data, group_size, strategy_pattern) {
    select_size <- nrow(select_data)
    generate_size <- group_size - select_size
    random_vector <- c()
    random <- sample(1:strategy_pattern, generate_size, replace = TRUE, prob = NULL)
    for (i in 1:generate_size) {
        random_vector <- append(random_vector, random[i])
    }
    return(random_vector)
}

#実行
roulette_new_generation_random_data <- gen_new_generation_random(
    select_data = roulette_select_data,
    group_size = group_size,
    strategy_pattern = strategy_pattern
)

ranking_new_generation_random_data <- gen_new_generation_random(
    select_data = ranking_select_data,
    group_size = group_size,
    strategy_pattern = strategy_pattern
)

###########################################################################################
#消えた分の生成
#割合で生成
each_strategy_ratio <- function(select_data, strategy_pattern) {
    sorted_strategy_data <- select_data[order(select_data$strategy_number, decreasing=F), ]
    each_strategy_data_items_vector <- c()
    for (i in 1:strategy_pattern) {
        #Headerがstrategy_numberのところで、strategy_numberがi
        filter <- sorted_strategy_data[sorted_strategy_data$strategy_number == i, ]
        #filterの行数を調べればいい
        each_strategy_data_items <- nrow(filter)
        each_strategy_data_items_vector <- append(each_strategy_data_items_vector, each_strategy_data_items)
    }
    return(each_strategy_data_items_vector)
}

#実行
roulette_each_strategy_data <- each_strategy_ratio(
    select_data = roulette_select_data,
    strategy_pattern = strategy_pattern
)

ranking_each_strategy_data <- each_strategy_ratio(
    select_data = ranking_select_data,
    strategy_pattern = strategy_pattern
)

#均等にする部分と変動する部分の割合
#!0 <= equal_ratio <= 1
equal_ratio <- 0.5
gen_new_generation_roulette <- function(equal_ratio, each_strategy_ratio, group_size, select_data, strategy_pattern) { # nolint
    #equal_ratioののエラー処理
    #!0 <= ratio <= 1
    if (0 > ratio || 1 < ratio) {
        stop('ratio can only input 0 <= ratio <= 1.')
    }
    #生成する分の長さ
    select_size <- nrow(select_data)
    generate_size <- group_size - select_size
    #均等にする部分の個数
    equal_ratio_size <- round(generate_size * equal_ratio)
    #変動する分の長さ
    fluctuant_ratio_size <- generate_size - equal_ratio_size
    #*実際の生成
    #均等にする部分の生成
    # equal_ratio_vector_prop_func <- function() {
    #     equal_ratio_vector_prop <- c()
    #     for (i in 1:strategy_pattern) {
    #         each_strategy_generate_ratio <- 100 / strategy_pattern
    #         equal_ratio_vector_prop <- append(equal_ratio_vector_prop, each_strategy_generate_ratio)
    #     }
    #     return(equal_ratio_vector_prop)
    # }
    # equal_ratio_vector_prob <- equal_ratio_vector_prop_func()
    equal_ratio_vector <- sample(1:strategy_pattern, equal_ratio_size, replace = TRUE, prob = NULL)
    #変動する分の生成
    fluctuant_ratio_vector <- sample(1:strategy_pattern, fluctuant_ratio_size, replace = TRUE, prob = each_strategy_ratio)
    #結果
    ratio_vector <- c(equal_ratio_vector, fluctuant_ratio_vector)
    return(ratio_vector)
}


#実行
roulette_new_generation_roulette_data <- gen_new_generation_roulette(
    equal_ratio = equal_ratio,
    each_strategy_ratio = roulette_each_strategy_data,
    group_size = group_size,
    select_data = roulette_select_data,
    strategy_pattern = strategy_pattern
)

ranking_new_generation_roulette_data <- gen_new_generation_roulette(
    equal_ratio = equal_ratio,
    each_strategy_ratio = ranking_each_strategy_data,
    group_size = group_size,
    select_data = ranking_select_data,
    strategy_pattern = strategy_pattern
)









#TODO: 結果の出力
create_new_generation <- function(select_data, new_generation_roulette_data) {
    new_generation_vector <- c(select_data$strategy_number, new_generation_roulette_data)
    return(new_generation_vector)
}

#割合で生成
roulette_new_generation <- create_new_generation(
    select_data = roulette_select_data,
    new_generation_roulette_data = roulette_new_generation_roulette_data
)

ranking_new_generation <- create_new_generation(
    select_data = ranking_select_data,
    new_generation_roulette_data = ranking_new_generation_roulette_data
)

#ランダムに生成
roulette_new_generation_random <- create_new_generation(
    select_data = roulette_select_data,
    new_generation_roulette_data = roulette_new_generation_random_data
)

ranking_new_generation_random <- create_new_generation(
    select_data = ranking_select_data,
    new_generation_roulette_data = ranking_new_generation_random_data
)