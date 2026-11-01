type t = {
  house_number : string option;
  road : string option;
  city : string option;
  town : string option;
  village : string option;
  county : string option;
  state : string option;
  postcode : string option;
  country : string option;
  country_code : string option;
  fields : Tags.t;
}

let jsont =
  Jsont.map Tags.jsont
    ~enc:(fun t -> t.fields)
    ~dec:(fun fields ->
      let get key = Tags.find key fields in
      {
        house_number = get "house_number";
        road = get "road";
        city = get "city";
        town = get "town";
        village = get "village";
        county = get "county";
        state = get "state";
        postcode = get "postcode";
        country = get "country";
        country_code = get "country_code";
        fields;
      })

let find key t = Tags.find key t.fields
