open Unicode.Std

module Make =
  functor(V : Hashtbl.HashedType) ->
  struct
    module ValueHashtbl = Hashtbl.Make(V)

    type t = {
              values  : string         ValueHashtbl.t;
              names   : (string, unit) Hashtbl.t;
      mutable next_id : int;
    }

    let create () =
      {
        values  = ValueHashtbl.create 10;
        names   = Hashtbl.create 10;
        next_id = 1;
      }

    let bind env value name =
      let name =
        if Hashtbl.mem env.names name then
          let mangled = name ^ "." ^ (string_of_int env.next_id) in
            assert (not (Hashtbl.mem env.names mangled));
            env.next_id <- env.next_id + 1;
            mangled
        else name
      in begin
        ValueHashtbl.add env.values value name;
        Hashtbl.add env.names name ();
        name
      end

    let get_exn env value =
      ValueHashtbl.find env.values value

    let get env value =
      try
        Some (ValueHashtbl.find env.values value)
      with Not_found ->
        None
  end
