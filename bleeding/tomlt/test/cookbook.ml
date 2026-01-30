(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

[@@@warning "-32"]

(** Cookbook examples - runnable implementations matching doc/cookbook.mld *)

(* ============================================
   Configuration Files
   ============================================ *)

module Config_files = struct
  (* Basic Configuration *)
  type database_config = {
    host : string;
    port : int;
    name : string;
  }

  let database_config_codec =
    Tomlt.(Table.(
      obj (fun host port name -> { host; port; name })
      |> mem "host" string ~enc:(fun c -> c.host)
      |> mem "port" int ~enc:(fun c -> c.port)
      |> mem "name" string ~enc:(fun c -> c.name)
      |> finish
    ))

  let example_database_toml = {|
host = "localhost"
port = 5432
name = "myapp"
|}

  (* Nested Configuration *)
  type server_config = {
    host : string;
    port : int;
  }

  type app_config = {
    name : string;
    server : server_config;
    debug : bool;
  }

  let server_config_codec =
    Tomlt.(Table.(
      obj (fun host port -> { host; port })
      |> mem "host" string ~enc:(fun s -> s.host)
      |> mem "port" int ~enc:(fun s -> s.port)
      |> finish
    ))

  let app_config_codec =
    Tomlt.(Table.(
      obj (fun name server debug -> { name; server; debug })
      |> mem "name" string ~enc:(fun c -> c.name)
      |> mem "server" server_config_codec ~enc:(fun c -> c.server)
      |> mem "debug" bool ~enc:(fun c -> c.debug)
      |> finish
    ))

  let example_app_toml = {|
name = "My Application"
debug = false

[server]
host = "0.0.0.0"
port = 8080
|}

  (* Multi-Environment Configuration *)
  type env_config = {
    database_url : string;
    log_level : string;
    cache_ttl : int;
  }

  type config = {
    app_name : string;
    development : env_config;
    production : env_config;
  }

  let env_config_codec =
    Tomlt.(Table.(
      obj (fun database_url log_level cache_ttl ->
        { database_url; log_level; cache_ttl })
      |> mem "database_url" string ~enc:(fun e -> e.database_url)
      |> mem "log_level" string ~enc:(fun e -> e.log_level)
      |> mem "cache_ttl" int ~enc:(fun e -> e.cache_ttl)
      |> finish
    ))

  let config_codec =
    Tomlt.(Table.(
      obj (fun app_name development production ->
        { app_name; development; production })
      |> mem "app_name" string ~enc:(fun c -> c.app_name)
      |> mem "development" env_config_codec ~enc:(fun c -> c.development)
      |> mem "production" env_config_codec ~enc:(fun c -> c.production)
      |> finish
    ))

  let example_multi_env_toml = {|
app_name = "MyApp"

[development]
database_url = "postgres://localhost/dev"
log_level = "debug"
cache_ttl = 60

[production]
database_url = "postgres://prod-db/app"
log_level = "error"
cache_ttl = 3600
|}
end

(* ============================================
   Optional and Absent Values
   ============================================ *)

module Optional_values = struct
  (* Default Values with dec_absent *)
  type settings = {
    theme : string;
    font_size : int;
    show_line_numbers : bool;
  }

  let settings_codec =
    Tomlt.(Table.(
      obj (fun theme font_size show_line_numbers ->
        { theme; font_size; show_line_numbers })
      |> mem "theme" string ~enc:(fun s -> s.theme)
          ~dec_absent:"default"
      |> mem "font_size" int ~enc:(fun s -> s.font_size)
          ~dec_absent:12
      |> mem "show_line_numbers" bool ~enc:(fun s -> s.show_line_numbers)
          ~dec_absent:true
      |> finish
    ))

  let example_settings_toml = {|
theme = "dark"
|}

  (* Option Types with opt_mem *)
  type user = {
    name : string;
    email : string option;
    phone : string option;
  }

  let user_codec =
    Tomlt.(Table.(
      obj (fun name email phone -> { name; email; phone })
      |> mem "name" string ~enc:(fun u -> u.name)
      |> opt_mem "email" string ~enc:(fun u -> u.email)
      |> opt_mem "phone" string ~enc:(fun u -> u.phone)
      |> finish
    ))

  let example_user_toml = {|
name = "Alice"
email = "alice@example.com"
|}

  (* Conditional Omission with enc_omit *)
  type retry_config = {
    name : string;
    retries : int;
  }

  let retry_config_codec =
    Tomlt.(Table.(
      obj (fun name retries -> { name; retries })
      |> mem "name" string ~enc:(fun c -> c.name)
      |> mem "retries" int ~enc:(fun c -> c.retries)
          ~dec_absent:0
          ~enc_omit:(fun r -> r = 0)
      |> finish
    ))
end

(* ============================================
   Datetimes
   ============================================ *)

module Datetimes = struct
  (* Basic Datetime Handling *)
  type event = { name : string; timestamp : Ptime.t }

  let event_codec =
    Tomlt.(Table.(
      obj (fun name timestamp -> { name; timestamp })
      |> mem "name" string ~enc:(fun e -> e.name)
      |> mem "when" (ptime ()) ~enc:(fun e -> e.timestamp)
      |> finish
    ))

  let example_event_toml = {|
name = "Meeting"
when = 2024-01-15T10:30:00Z
|}

  (* Strict Timestamp Validation *)
  type audit_log = { action : string; timestamp : Ptime.t }

  let audit_codec =
    Tomlt.(Table.(
      obj (fun action timestamp -> { action; timestamp })
      |> mem "action" string ~enc:(fun a -> a.action)
      |> mem "timestamp" (ptime_opt ()) ~enc:(fun a -> a.timestamp)
      |> finish
    ))

  let example_audit_toml = {|
action = "user_login"
timestamp = 2024-01-15T10:30:00Z
|}

  (* Date-Only Fields *)
  type person = { name : string; birthday : Ptime.date }

  let person_codec =
    Tomlt.(Table.(
      obj (fun name birthday -> { name; birthday })
      |> mem "name" string ~enc:(fun p -> p.name)
      |> mem "birthday" ptime_date ~enc:(fun p -> p.birthday)
      |> finish
    ))

  let example_person_toml = {|
name = "Bob"
birthday = 1985-03-15
|}

  (* Time-Only Fields *)
  type alarm = { label : string; time : Ptime.Span.t }

  let alarm_codec =
    Tomlt.(Table.(
      obj (fun label time -> { label; time })
      |> mem "label" string ~enc:(fun a -> a.label)
      |> mem "time" ptime_span ~enc:(fun a -> a.time)
      |> finish
    ))

  let example_alarm_toml = {|
label = "Wake up"
time = 07:30:00
|}

  (* Preserving Datetime Format *)
  type flexible_event = {
    name : string;
    when_ : Tomlt.Toml.ptime_datetime;
  }

  let flexible_codec =
    Tomlt.(Table.(
      obj (fun name when_ -> { name; when_ })
      |> mem "name" string ~enc:(fun e -> e.name)
      |> mem "when" (ptime_full ()) ~enc:(fun e -> e.when_)
      |> finish
    ))

  let example_flexible_toml = {|
name = "Birthday"
when = 1985-03-15
|}
end

(* ============================================
   Arrays
   ============================================ *)

module Arrays = struct
  (* Basic Arrays *)
  type network_config = {
    name : string;
    ports : int list;
    hosts : string list;
  }

  let network_config_codec =
    Tomlt.(Table.(
      obj (fun name ports hosts -> { name; ports; hosts })
      |> mem "name" string ~enc:(fun c -> c.name)
      |> mem "ports" (list int) ~enc:(fun c -> c.ports)
      |> mem "hosts" (list string) ~enc:(fun c -> c.hosts)
      |> finish
    ))

  let example_network_toml = {|
name = "load-balancer"
ports = [80, 443, 8080]
hosts = ["web1.example.com", "web2.example.com"]
|}

  (* Arrays of Tables *)
  type product = { name : string; price : float }
  type catalog = { products : product list }

  let product_codec =
    Tomlt.(Table.(
      obj (fun name price -> { name; price })
      |> mem "name" string ~enc:(fun p -> p.name)
      |> mem "price" float ~enc:(fun p -> p.price)
      |> finish
    ))

  let catalog_codec =
    Tomlt.(Table.(
      obj (fun products -> { products })
      |> mem "products" (array_of_tables product_codec)
          ~enc:(fun c -> c.products)
      |> finish
    ))

  let example_catalog_toml = {|
[[products]]
name = "Widget"
price = 9.99

[[products]]
name = "Gadget"
price = 19.99
|}

  (* Nested Arrays *)
  type matrix = { rows : int list list }

  let matrix_codec =
    Tomlt.(Table.(
      obj (fun rows -> { rows })
      |> mem "rows" (list (list int)) ~enc:(fun m -> m.rows)
      |> finish
    ))

  let example_matrix_toml = {|
rows = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
|}
end

(* ============================================
   Tables
   ============================================ *)

module Tables = struct
  (* Inline Tables *)
  type point = { x : int; y : int }

  let point_codec =
    Tomlt.(Table.(
      obj (fun x y -> { x; y })
      |> mem "x" int ~enc:(fun p -> p.x)
      |> mem "y" int ~enc:(fun p -> p.y)
      |> inline
    ))

  (* Deeply Nested Structures *)
  type address = { street : string; city : string }
  type company = { name : string; address : address }
  type employee = { name : string; company : company }

  let address_codec =
    Tomlt.(Table.(
      obj (fun street city -> { street; city })
      |> mem "street" string ~enc:(fun (a : address) -> a.street)
      |> mem "city" string ~enc:(fun a -> a.city)
      |> finish
    ))

  let company_codec =
    Tomlt.(Table.(
      obj (fun name address -> { name; address })
      |> mem "name" string ~enc:(fun (c : company) -> c.name)
      |> mem "address" address_codec ~enc:(fun c -> c.address)
      |> finish
    ))

  let employee_codec =
    Tomlt.(Table.(
      obj (fun name company -> { name; company })
      |> mem "name" string ~enc:(fun (e : employee) -> e.name)
      |> mem "company" company_codec ~enc:(fun e -> e.company)
      |> finish
    ))

  let example_employee_toml = {|
name = "Alice"

[company]
name = "Acme Corp"

[company.address]
street = "123 Main St"
city = "Springfield"
|}
end

(* ============================================
   Unknown Members
   ============================================ *)

module Unknown_members = struct
  (* Ignoring Unknown Members (Default) *)
  let host_only_codec =
    Tomlt.(Table.(
      obj (fun host -> host)
      |> mem "host" string ~enc:Fun.id
      |> skip_unknown
      |> finish
    ))

  (* Rejecting Unknown Members *)
  let strict_config_codec =
    Tomlt.(Table.(
      obj (fun host port -> (host, port))
      |> mem "host" string ~enc:fst
      |> mem "port" int ~enc:snd
      |> error_unknown
      |> finish
    ))

  (* Collecting Unknown Members *)
  type extensible_config = {
    name : string;
    extra : (string * Tomlt.Toml.t) list;
  }

  let extensible_config_codec =
    Tomlt.(Table.(
      obj (fun name extra -> { name; extra })
      |> mem "name" string ~enc:(fun c -> c.name)
      |> keep_unknown (Mems.assoc value) ~enc:(fun c -> c.extra)
      |> finish
    ))

  let example_extensible_toml = {|
name = "app"
foo = 42
bar = "hello"
|}

  (* Typed Unknown Members *)
  module StringMap = Map.Make(String)

  type translations = {
    default_lang : string;
    strings : string StringMap.t;
  }

  let translations_codec =
    Tomlt.(Table.(
      obj (fun default_lang strings -> { default_lang; strings })
      |> mem "default_lang" string ~enc:(fun t -> t.default_lang)
      |> keep_unknown (Mems.string_map string) ~enc:(fun t -> t.strings)
      |> finish
    ))

  let example_translations_toml = {|
default_lang = "en"
hello = "Hello"
goodbye = "Goodbye"
thanks = "Thank you"
|}
end

(* ============================================
   Validation
   ============================================ *)

module Validation = struct
  (* Range Validation with iter *)
  let port_codec =
    Tomlt.(iter int
      ~dec:(fun p ->
        if p < 0 || p > 65535 then
          failwith "port must be between 0 and 65535"))

  let percentage_codec =
    Tomlt.(iter float
      ~dec:(fun p ->
        if p < 0.0 || p > 100.0 then
          failwith "percentage must be between 0 and 100"))

  (* String Enumerations *)
  type log_level = Debug | Info | Warning | Error

  let log_level_codec =
    Tomlt.enum [
      "debug", Debug;
      "info", Info;
      "warning", Warning;
      "error", Error;
    ]

  type log_config = { level : log_level }

  let log_config_codec =
    Tomlt.(Table.(
      obj (fun level -> { level })
      |> mem "level" log_level_codec ~enc:(fun c -> c.level)
      |> finish
    ))

  let example_log_toml = {|
level = "info"
|}
end

(* ============================================
   Recursion
   ============================================ *)

module Recursion = struct
  type tree = Node of int * tree list

  let rec tree_codec = lazy Tomlt.(
    Table.(
      obj (fun value children -> Node (value, children))
      |> mem "value" int ~enc:(function Node (v, _) -> v)
      |> mem "children" (list (rec' tree_codec))
          ~enc:(function Node (_, cs) -> cs)
          ~dec_absent:[]
      |> finish
    ))

  let tree_codec = Lazy.force tree_codec

  let example_tree_toml = {|
value = 1

[[children]]
value = 2

[[children]]
value = 3

[[children.children]]
value = 4
|}
end

(* ============================================
   Main - Run examples
   ============================================ *)

let decode_and_print name codec toml =
  Printf.printf "=== %s ===\n" name;
  match Tomlt_bytesrw.decode_string codec toml with
  | Ok _ -> Printf.printf "OK: Decoded successfully\n\n"
  | Error e -> Printf.printf "ERROR: %s\n\n" (Tomlt.Toml.Error.to_string e)

let () =
  Printf.printf "Tomlt Cookbook Examples\n";
  Printf.printf "=======================\n\n";

  (* Config files *)
  decode_and_print "Database config"
    Config_files.database_config_codec
    Config_files.example_database_toml;

  decode_and_print "App config"
    Config_files.app_config_codec
    Config_files.example_app_toml;

  decode_and_print "Multi-env config"
    Config_files.config_codec
    Config_files.example_multi_env_toml;

  (* Optional values *)
  decode_and_print "Settings with defaults"
    Optional_values.settings_codec
    Optional_values.example_settings_toml;

  decode_and_print "User with optional fields"
    Optional_values.user_codec
    Optional_values.example_user_toml;

  (* Datetimes *)
  decode_and_print "Event with datetime"
    Datetimes.event_codec
    Datetimes.example_event_toml;

  decode_and_print "Audit log (strict)"
    Datetimes.audit_codec
    Datetimes.example_audit_toml;

  decode_and_print "Person with birthday"
    Datetimes.person_codec
    Datetimes.example_person_toml;

  decode_and_print "Alarm with time"
    Datetimes.alarm_codec
    Datetimes.example_alarm_toml;

  decode_and_print "Flexible event"
    Datetimes.flexible_codec
    Datetimes.example_flexible_toml;

  (* Arrays *)
  decode_and_print "Network config"
    Arrays.network_config_codec
    Arrays.example_network_toml;

  decode_and_print "Product catalog"
    Arrays.catalog_codec
    Arrays.example_catalog_toml;

  decode_and_print "Matrix"
    Arrays.matrix_codec
    Arrays.example_matrix_toml;

  (* Tables *)
  decode_and_print "Employee (nested)"
    Tables.employee_codec
    Tables.example_employee_toml;

  (* Unknown members *)
  decode_and_print "Extensible config"
    Unknown_members.extensible_config_codec
    Unknown_members.example_extensible_toml;

  decode_and_print "Translations"
    Unknown_members.translations_codec
    Unknown_members.example_translations_toml;

  (* Validation *)
  decode_and_print "Log config"
    Validation.log_config_codec
    Validation.example_log_toml;

  (* Recursion *)
  decode_and_print "Tree"
    Recursion.tree_codec
    Recursion.example_tree_toml;

  Printf.printf "All examples completed.\n"
