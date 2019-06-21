# ---- cow_template ----

#' A data.table to store status of cows
#'
#' `cow_table` is a [data.table::data.table] to store status of cows.
#' Status of all the cows in a farm is stored in one `cow_table`.
#' The rows are consists of `a_new_calf`, which indicates one cow.
#'
#' `date_xxx` variables are expressed as month. The month when the simulation starts is 0. eg. The `date_birth` of a cow which is 20 months old at the simulation start is -20.
#' `day_xxx` variables are days in the month.
#'
#' - `row_id`: Fixed.
#' - `cow_id`
#' - `age`: Age in month.
#' - `stage`: One of "calf", "heifer", "milking" or "dry".
#' - `sex`: One of "female" (Holstern), "male" (Holstein), "freemartin" (female Hostein), "f1-female" (Hostein x Japanese black), "f1-male" (Holstein x Japanese black), "black-female" (Japanese black), "black-male" (Japanese black)
#' - `date_birth`: Date of birth.
#' - `date_death`: Date of death. The value is NA while the cow is alive. It is set after the cow died.
#' - `date_death_expected`: Expected date of death. It is calculated when a calf is born or when a cow is introduced to the herd.
#' - `is_owned`: Whether the cow is owned by the simulated herd. It is set FALSE when the cow is sold or died.
#' - `cause_removal`:
#'     - "died": culled or slaughtered
#'     - "ebl": culled due to onset of the disease
#'     - "sold": send to a market or another farm
#      - "will_die": (still alive and) will die
#      - "will_be_slaughtered": (still alive and) will be slaughtered
#' - `is_replacement`:
#'     TRUE: The cow will be kept in the farm as replacement.
#'     FALSE: The cow Will be sold to beef operations.
#' - `is_introduced`
#' - `is_in_common_ranch`: Whether the cow is in a common ranch at `i_month` (see below).
#' - `is_grazed`
#' - `parity`
#' - `date_last_delivery`: 'Delivery' here includes abortions and stillbirths.
#' - `date_got_pregnant`: NA means that the cow is open.
#' - `n_ai`: The number of AI from last delivery. The value is set as 0 when the cow got pregnant.
#' - `day_heat`: Day in month of the NEXT heat.
#' - `day_last_heat`: Day in month of the LAST heat.
#' - `day_last_heat_detected`: Day in month of the last DETECTED heat.
#' - `n_heat_from_ai`: The number of heat from the last AI.
#' - `infection_status`: One of "s" (susceptible = non-infected), "ial" (aleukemia), "ipl" (PL) and "ebl".
#' - `date_ial`: The month when `infection_status` changes from "s" to "ial".
#' - `date_ipl`: The month when `infection_status` changes from "ial" to "ipl".
#' - `date_ipl_expected`: The expected month when `infection_status` changes from "ial" to "ipl". It is calculated when `infection_status` becomes "ial".
#' - `date_ebl`: The month when `infection_status` changes from "ipl" to "ebl".
#' - `date_ebl_expected`: The exppected month when `infection_status` changes from "ial" to "ebl". It is calculated when `infection_status` becomes "ial".
#' - `cause_infection`: The reason why the cow is infected. It is set when the cow gets infected.
#'     - "initial": already be infected at the start of the start of a simulation
#'     - "insects": by bloodsucking insects
#'     - "contact": by direct contact with an infected cow
#'     - "needles": by a contaminated needle
#'     - "rp": by rectal palpation
#'     - "vertical": vertical transimisson (intrauterine and by the dam's milk)
#'     - "introduced": for a introduced cow which is infected before introduction (a home-bred cow infected from introduced cow is not categorized as this)
#'     - "comranch": infected at a communal ranch
#' - `susceptibility_ial_to_ipl`: Genetic susceptibility to disease progress (Ial -> Ipl)
#' - `susceptibility_ipl_to_ebl`: Genetic susceptibility to disease progress (Ipl -> EBL)
#' -`group_id`: Barn ID
#' -`chamber_id`: ID of the chamber in which the cow kept for a cow in a tie-stall barn. `NA_real_` for a cow in a free-stall barn.
#' -`is_isolated`: Whether the cow is isolated for a cow in a tie-stall barn. `NA_real_` for a cow in a free-stall barn.
#' -`i_month`: The number of months past from the start of a simulation.
#'
#' @format [data.table::data.table]
#' @seealso [barn_table]
#' @name cow_table
#' @export
NULL

#' @rdname cow_table
#' @export
a_new_calf <- data.table(
  row_id = NA_integer_,
  # TODO:これもう使わないから削除。
  cow_id = NA_integer_,
  age = NA_real_,
  stage = NA_character_,
  sex = NA_character_,
  date_birth = NA_real_,
  date_death = NA_real_,
  # TODO: date_removal が必要か？ もしそうならdate_deathと統合するべきか？
  date_death_expected = NA_real_,
  is_owned = NA,
  cause_removal = NA_character_,
  # TODO: 他の感染症に応用することも考えて、eblはonsetとかに名前を変えたい。
  is_replacement = NA,
  is_introduced = NA,
  is_in_common_ranch = NA,
  is_grazed = NA,

  # Delivery and milking
  parity = NA_real_,
  date_last_delivery = NA_real_,
  date_got_pregnant = NA_real_,
  n_ai = NA_real_,
  day_heat = NA_real_,
  day_last_heat = NA_real_,
  day_last_heat_detected = NA_real_,
  # TODO: day_heat, 妊娠したらNAにすることにしよ。発情中のみセットされてる方がわかりやすい。
  # TODO: ここなんのためにday_heatとday_last_heatセットしたんだっけ？　もっとシンプルにできない？
  # TODO: day_heat_シリーズ、発情のない牛にもセットすることにしてるけど、発情が来てからセットしたほうがわかりやすいな

  n_heat_from_ai = NA_real_,

  # Infection status
  infection_status = NA_character_,
  date_ial = NA_real_,
  date_ipl = NA_real_,
  date_ipl_expected = NA_real_,
  date_ebl = NA_real_,
  date_ebl_expected = NA_real_,
  cause_infection = NA_character_,
  susceptibility_ial_to_ipl = NA,
  # TODO: これ削除
  susceptibility_ipl_to_ebl = NA,
  # TODO: これ削除

  group_id = NA_integer_,  # TODO: Rename to barn_id

  # For tie-stall (For free-stall, all the following variables are NA)
  chamber_id = NA_integer_,
  is_isolated = NA,
  i_month = NA_real_
)


## ---- chamber_template ----

#' Barn data.table to store status of barns
#'
#' `barn_table` is a [data.table::data.table] to store status of barns.
#' Each barn have each `barn_table`.
#' The rows are consists of `a_chamber`, which indicates one chamber in a barn.
#'
#' `chamber_id`, `is_edge`, and `group_id` are fixed. Values will not be changed while a simulation.
#' Other variables are flexible. Values will may be changed while a simulation.
#'
#' - `chamber_id`: Chamber ID.
#' - `is_edge1`: Whether the chamber is at a right end of a lane.
#' - `is_edge2`: Whether the chamer is at a left end of a lane.
#' - `cow_id`: Cow ID in a lane.
#' - `cow_status`: Infection status of the cow.
#' - `is_exposed`:
#'     TRUE = The cow is not isolated AND one or both of the neighbor cows is infected and not isolated
#'     FALSE = Both of the neibors are either uninfected or isolated
#'     NA = No cow in the chamber
#' - `is_isolated`: Whether the cow is isolated or not
#' - `hazard_ratio`: Hazard ratio of infection to the cow calculated based on neighbors' infection status.
#' - `neighbor1_status`: Infection status of the neighbor in the right chamber.
#' - `neighbor1_isolated`: Whether the cow in the right chamber is isolated or not.
#' - `neighbor1_infectivity`:
#'     TRUE when the neighbor in the right chamber is not isolated and is infectious. Otherwise, FALSE. NA is not allowed to this variable.
#' - `neighbor2_status`, `neighbor2_isolated`, `neighbor2_infectivity`: Variables about the neighbor in the left chamber.
#'
#' @format [data.table::data.table]
#' @seealso [cow_table]
#' @name barn_table
#' @export
NULL

#' @rdname barn_table
#' @export
a_chamber <- data.table(
  chamber_id = NA_integer_,
  is_edge1 = NA,
  is_edge2 = NA,

  cow_id = NA_integer_,
  cow_status = NA_character_,
  is_exposed = NA,
  is_isolated = NA,
  hazard_ratio = NA_real_,

  neighbor1_status = NA_character_,
  neighbor1_isolated = NA,
  neighbor1_infectivity = F,
  # TODO: ここ自分がisolatedかどうかは無視することになってる。ややこしいので変えたい
  # TODO: ここもneighbor1がいないときはNA、みたいに変える
  neighbor2_status = NA_character_,
  neighbor2_isolated = NA,
  neighbor2_infectivity = F,

  group_id = NA_integer_
)



## ---- rectal_palpation_template ----

#' A data.table to manage cow status related with rectal palpation
#'
#' `rp_table` is a [data.table::data.table] to manage cow status related with rectal palpation.
#' The table is automatically made and used within a simulation. You don't have to make it manually.
#' The rows are consists of `day_rp`, which indicates one chance of rectal palpation for one cow.
#'
#' - `cow_id`, `infection_status`: See [cow_table].
#' - `day_rp`: Day in month when rectal palpation is conducted.
#' - `i_rp`: When more than two cows are rectally palpated in the same day due to the same reason (see `type` below), each cow is palpaded on `i_rp` th turn.
#' - `is_after_inf`: Whether a cow palpated on `i_rp` - 1 th turn is infected or not.
#' - `is_infected`: Whether a cow is infected due to the rectal palpation.
#'
#' @format [data.table::data.table]
#' @seealso [cow_table] [barn_table]
#' @name rp_table
#' @export
NULL

#' @rdname rp_table
#' @export
day_rp <- data.table(cow_id = NA_integer_,
                     infection_status = NA_character_,
                     day_rp = NA_real_,
                     type = NA_character_,
                     i_rp = NA_integer_,
                     is_after_inf = NA,
                     is_infected = NA)


