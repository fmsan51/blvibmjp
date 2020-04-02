# ---- cow_template ----

#' A data.table to store status of cows
#'
#' `cow_table` is a [data.table][data.table::data.table] to store status of cows.
#' Status of all the cows in a farm is stored in one `cow_table`.
#' The rows are consists of `a_new_calf`, which indicates one cow.
#'
#' `date_xxx` variables are expressed as month. The month when the simulation starts is 0. eg. The `date_birth` of a cow which is 20 months old at the simulation start is -20.
#' `day_xxx` variables are days in the month.
#'
#' - `cow_id`
#' - `age`: Age in month.
#' - `stage`: One of "calf", "heifer", "milking" or "dry".
#' - `sex`: One of "female" (Holstern), "male" (Holstein), "freemartin" (female Hostein), "f1-female" (Hostein x Japanese black), "f1-male" (Holstein x Japanese black), "black-female" (Japanese black), "black-male" (Japanese black).
#' - `date_birth`: Date of birth.
#' - `date_death`: Date of death. The value is NA while the cow is alive. It is set after the cow died.
#' - `date_death_expected`: Expected date of death. It is calculated when a calf is born or when a cow is introduced to the herd.
#' - `is_owned`: Whether the cow is owned by the simulated herd. It is set FALSE when the cow is sold or died.
#' - `cause_removal`:
#'     - "died": died.
#'     - "slaughtered": slaughtered not due to the disease.
#'     - "culled": culled due to the disease.
#'     - "sold": send to a market or another farm.
#'     - "will_die": (still alive and) will die.
#'     - "will_be_slaughtered": (still alive and) will be slaughtered.
#' - `is_replacement`:
#'     TRUE: The cow will be kept in the farm as replacement.
#'     FALSE: The cow Will be sold to beef operations.
#' - `parity`
#' - `date_last_delivery`: 'Delivery' here includes abortions and stillbirths. `NA` means that the cow is heifer (pairty = 0).
#' - `date_got_pregnant`: NA means that the cow is open.
#' - `date_dried`: NA means that the cow is milking.
#' - `n_ai`: The number of AI from last delivery. The value is set as 0 when the cow got pregnant.
#' - `day_heat`: Day in month of the NEXT heat.
#' - `day_last_detected_heat`: Day in month of the LAST heat.
#' - `is_to_test_pregnancy`: Whether to be served for a pregnancy test (= AI was conducted to the cow and heats were not observed from then).
#' - `infection_status`: One of "s" (susceptible = non-infected), "ial" (aleukemia), "ipl" (PL) and "ebl".
#' - `is_detected`: Whether BLV infection is detected. Sometimes even a non-infected cow can be `TRUE` when false-positive in BLV test occurs.
#' - `date_ial`: The month when `infection_status` changes from "s" to "ial".
#' - `date_ipl`: The month when `infection_status` changes from "ial" to "ipl".
#' - `date_ipl_expected`: The expected month when `infection_status` changes from "ial" to "ipl". It is calculated when `infection_status` becomes "ial".
#' - `date_ebl`: The month when `infection_status` changes from "ipl" to "ebl".
#' - `date_ebl_expected`: The exppected month when `infection_status` changes from "ial" to "ebl". It is calculated when `infection_status` becomes "ial".
#' - `cause_infection`: The reason why the cow is infected. It is set when the cow gets infected.
#'     - "initial": already be infected at the start of the start of a simulation.
#'     - "insects": by bloodsucking insects.
#'     - "contact": by direct contact with an infected cow.
#'     - "needles": by a contaminated needle.
#'     - "rp": by rectal palpation.
#'     - "vertical": vertical transimisson (intrauterine and by the dam milk).
#'     - "introduced": for a introduced cow which is infected before introduction (a home-bred cow infected from introduced cow is not categorized as this).
#'     - "communal_pasture": infected at a communal pasture.
#' - `susceptibility_ial_to_ipl`: Genetic susceptibility to disease progress (Ial -> Ipl).
#' - `susceptibility_ipl_to_ebl`: Genetic susceptibility to disease progress (Ipl -> EBL).
#' - `area_id`: Area ID. `0` means that the cow is in a communal pasture.
#' - `months_in_area`: The number of month a cow stayed in the current area.
#' - `chamber_id`: ID of the chamber in which the cow kept for a cow in a tie-stall barn. `NA_real_` for a cow in a free-stall barn. `0` for a cow in a tie-stall barn but to it a chamber is not assigned (a free-roaming cow).
#' - `is_isolated`: Whether the cow is isolated for a cow in a tie-stall barn. `NA_real_` for a cow in a free-stall barn.
#' - `i_month`: The number of months past from the start of a simulation.
#'
#' @format [data.table][data.table::data.table]
#' @seealso [tie_stall_table] [area_table] [movement_table] [rp_table]
#'
#' @name cow_table
#' @export
a_new_calf <- data.table(
  # TODO: Add notes indicating which parameter is necessary and which is not.
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

  # Delivery and milking
  parity = NA_real_,
  date_last_delivery = NA_real_,
  date_got_pregnant = NA_real_,
  date_dried = NA_real_,  # TODO: make function to set this
  # n_ai is currently used nowhere,
  # but recorded for when to consider repeat breeder.
  n_ai = NA_real_,
  day_heat = NA_real_,
  day_last_detected_heat = NA_real_,
  # day_last_detected_heat is used to calculate day of pregnancy test.
  # TODO: day_heat, 妊娠したらNAにすることにしよ。発情中のみセットされてる方がわかりやすい。
  # TODO: day_heat_シリーズ、発情のない牛にもセットすることにしてるけど、発情が来てからセットしたほうがわかりやすいな

  is_to_test_pregnancy = NA,

  # Infection status
  infection_status = NA_character_,
  is_detected = NA,
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

  area_id = NA_integer_,
  months_in_area = NA_real_,  # TODO: make a function to increment this

  # For tie-stall (For free-stall, all the following variables are NA)
  chamber_id = NA_integer_,  # TODO: Remove chamber_id from cow_table. It's enough if tie_stall_table holds chamber_id.
  is_isolated = NA,
  i_month = NA_real_
)
# TODO: consider whether status can removed from the codes.


# ---- tie_stall_template ----

#' A data.table to store status of a tie-stall barn
#'
#' `tie_stall_table` is a [data.table][data.table::data.table] to store status of tie-stall barns.
#' Each tie-stall barn have each `tie_stall_table`.
#' The rows are consisted of `a_chamber`, which indicates one chamber in a barn.
#'
#' `chamber_id` and `adjoin_previous/next_chamber` are fixed. Values will not be changed while a simulation.
#' Other variables are flexible. Values will may be changed while a simulation.
#'
#' - `chamber_id`: Chamber ID.
#' - `adjoin_previous_chamber`: Whether the chamber adjoins the `chamber_id - 1`th chamber.
#' - `adjoin_next_chamber`: Whether the chamber adjoins the `chamber_id + 1`th chamber.
#' - `cow_id`: Cow ID in a lane.
#' - `cow_status`: Infection status of the cow.
#' - `is_isolated`: Whether the cow is isolated or not.
#'
#' @format [data.table][data.table::data.table]
#' @seealso [cow_table] [area_table] [movement_table] [rp_table]
#' @name tie_stall_table
#' @export
a_chamber <- data.table(
  chamber_id = NA_integer_,  # NOTE: use NA_real_ and specify obstacles by 0.5
  adjoint_previous_chamber = NA,
  adjoint_next_chamber = NA,

  cow_id = NA_integer_,
  cow_status = NA_character_,
  is_isolated = NA
)


# ---- area_template ----

#' A data.table to manage areas in a farm
#'
#' `area_table` is a [data.table][data.table::data.table] to manage areas (barns, paddocks, hatches, etc.) in a farm.
#' Users must specify one `area_table` consisted of following items before starting a simulation.
#'
#' - `area_id` (integer): Area ID.
#' - `area_type` (`"free"`/`"tie"`/`"outside"`/`"hatch"`/`"communal pasture"`): Type of a area. Specify one of `"free"` (hatch, freebarn, free-stall, etc.), `"tie"` (tie-stall), `"outside"` (paddock or rangeland, etc.), `"hatch"` or `"communal pasture"` (yotaku).
#' - `capacity` (list consisted of numeric): Max number of cows to be kept in the area. `Inf` is set if you specify `NA`. `capacity` must be set if `area_type` is `"tie"`; otherwise optional.
#'   - If `area_type` is `"free"` or `"outside"`: a numeric or `NA`.
#'   - If `area_type` is `"tie"`: a numeric vector whose length is equal to the number of lanes in the area and each elements indicates the number of chambers in a lane.
#'   - If `area_type` is `"hatch"`: only `NA` is allowed.
#'
#' @note
#' Several parameters are calculated by [setup_area_table] and added to a `area_table` as attribute variables. Such values are intenended to be touched only by simulation functions and not by users.
#' - `capacity`: Max number of cows can be kept in a area.
#' - `tie_stall`: `area_id`s of tie-stall areas.
#'
#' @examples
#' # A farm has three areas:
#' # - A freebarn for calves (max capacity is 30 calves).
#' # - A paddock for heifers (capacity is not limited).
#' # - A tie-stall for delivered cows consisted of 2 lanes with 40 chambers and 2 lanes with 30 chambers.
#' areas <- a_area[rep(1, 3), ]
#' areas[, `:=`(area_id = 1:3,
#'              area_type = c("free", "outside", "tie"),
#'              capacity = list(30, NA, c(40, 40, 30, 30))]
#' @seealso [cow_table] [tie_stall_table] [movement_table] [rp_table]
#' @name area_table
#' @export
a_area <- data.table(area_id = NA_integer_,
                     area_type = NA_character_,
                     capacity = list(NA))
# TODO: How to hundle when multiple cows are set to one hatch


# ---- movement_template ----

#' A data.table to manage cows' movement between areas
#'
#' `movement_table` is a [data.table][data.table::data.table] to manage cow movement among areas.
#' Users must specify one `movement_table` consisted of following items before starting a simulation.
#'
#' - `current_area` (integer): The current area a cow is kept specified by `area_id` in [area_table].
#' - `condition` (character): Condition that cows in the area move to the next area(s). Describe conditions by character which can be evaluated as logical (see Example). You can use following terms to specify `condition`:
#'     - `age`: Age in month. Use like `age == 20`.
#'     - `parity`: Parity. Use like `parity > 1`.
#'     - `months_from_delivery`, `months_from_pregnancy`, `months_from_dry`: The number of months from delivery, pregnancy or dry. Use like `months_from_delivery == 0` (this means the month a cow delivered).
#'     - `delivery`, `pregnancy`, `dry`: A shorthand form of `months_from_delivery == 0` and so on.
#'     - `dim`: Days in milking. Use like `dim > 100`.
#'     - `month`: Month in a year (1 = Jan, 2 = Feb, ...). Use like `month == 3`.
#'     - `stay`: The number of months for which a cow stayed in a area. Use like `stay == 3`.
#' - `next_area` (list consisted of integer): The next area a cow will move to specified by `area_id` in [area_table]. You can specify multiple areas like `c(1:2, 4)`.
#' - `priority` (list consisted of integer and/or numeric): The priority for `next_area`. Specify integer or numeric vector (for numeric vector, they must be summed to 1,) whose length is equal to `next_area`. If `priority` is set by integer, cows move to the area with highest `priority` (= nearest to 1) which is not full. If multiple areas have the same `priority`, cows are romdomly allocated to the areas. If `priority` is set by numeric which is summed to 1, `priority` is regarded as probability in accordance to which cows move to `next_area`.
#'
#' If a cow meets multiple conditions, the condition in the fastest row will be used.
#'
#' @examples
#' movements <- a_movement[rep(1, 3), ]
#' movements[, `:=`(current_area = c(1L, 2L, 3L),
#'                  condition = c("age > 2", "delivery", "delivery"),
#'                  next_area = list(2L, 3:4, 3:4),
#'                  priority = list(NA, NA, c(1, 2)))]
#'
#' @seealso [cow_table] [tie_stall_table] [area_table] [rp_table]
#' @name movement_table
#' @export
a_movement <- data.table(current_area = NA_integer_,
                         condition = NA_character_,
                         next_area = list(NA),
                         priority = list(NA))
# TODO: Make UI to setup this.


# ---- rectal_palpation_template ----

#' A data.table to manage cow status related with rectal palpation
#'
#' `rp_table` is a [data.table][data.table::data.table] to manage cow status related with rectal palpation.
#' The table is automatically made and used within a simulation, therefore not intended to be touched by a user.
#' The rows are consists of `day_rp`, which indicates one chance of rectal palpation for one cow.
#'
#' - `cow_id`, `infection_status`: See [cow_table].
#' - `day_rp`: Day in month when rectal palpation is conducted.
#' - `i_rp`: When more than two cows are rectally palpated in the same day due to the same reason (see `type` below), each cow is palpaded on `i_rp`th turn.
#' - `type`: character. Type of pregnancy diagnosis specified by one of c("ai_am", "ai_pm", "pregnancy_test", "health_check" (health checking after a delivery).
#' - `is_after_inf`: Whether a cow palpated on `i_rp - 1`th turn is infected or not.
#' - `is_infected`: Whether a cow is infected due to the rectal palpation.
#'
#' @format [data.table] [data.table::data.table]
#' @seealso [cow_table] [tie_stall_table] [area_table] [movement_table]
#' @name rp_table
#' @export
one_day_rp <- data.table(cow_id = NA_integer_,
                         infection_status = NA_character_,
                         day_rp = NA_real_,
                         type = NA_character_,
                         i_rp = NA_integer_,
                         is_after_inf = NA,
                         is_infected = NA)
# TODO: 名前は検討

