# Counting Sundays
#
# You are given the following information, but you may prefer to do some research for yourself.
#
#    1 Jan 1900 was a Monday.
#    Thirty days has September,
#    April, June and November.
#    All the rest have thirty-one,
#    Saving February alone,
#    Which has twenty-eight, rain or shine.
#    And on leap years, twenty-nine.
#    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
#
# How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?



month30 <- function(m) {
    return (m==4 | m==6 | m==9 | m==11)
}

month29 <- function(y) {
    return (y %% 4 == 0 & y %% 100 != 0 | y %% 400 == 0)

}

sunday_firstday <- function(begin_year=1901, end_year=2000, start_day=1) {
    sundays <- 0
    day_of_week <- start_day
    for (year in begin_year : end_year) {
        for (month in 1 : 12) {
            days = 31
            if (month30(month)) {
                days = 30
            }
            if (month == 2) {
                days = 28
                if (month29(year)) {
                    days = 29
                }
            }
            for (day in 1 : days) {
                day_of_week <- day_of_week + 1
                if (day_of_week == 8) {
                    day_of_week <- 1
                }
                if (day_of_week == 1 & day == 1) {
                    sundays <- sundays + 1
                }
            }
        }
    }
    return (sundays)
}


print(sunday_firstday())



##
