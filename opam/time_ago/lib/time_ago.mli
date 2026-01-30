open! Core

(** {v
 Time-ago turns elapsed or future times into read-friendly time strings meant
    to be used in user interfaces where precision is not a major concern. The chart
    below shows the relationship between the magnitude of the timespan and the
    string generated.

    Conversion chart
    ----------------
    #  0s     30s    90s        45m      90m     18h     1d,4h     1d,20h
    #  |------|------|----------|--------|-------|-------|---------|---------->
    #      |      |        |        |        |       |        |          |
    #      V      |        V        |        V       |        V          |
    #  "just now" |   "X minutes"   |    "X hours"   | "more than a day" |
    #             V                 V                V                   V
    #      "about a minute"  "about an hour"   "about a day"         "X days"
    v} *)

(** Constructs a read-friendly time string using [Time_ns.t] as the reference time. [?now]
    defaults to [Time_ns.now ()]. Now > reference denotes an event in the past, while now
    < reference denotes an event in the future. *)
val to_string : ?now:Time_ns.t -> Time_ns.t -> string
