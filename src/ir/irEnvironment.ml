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
        next_id = 0;
      }

    let bind env value name =
      let name =
        let mangled =
          if name = "" then begin
            env.next_id <- env.next_id + 1;
            string_of_int env.next_id
          end else if Hashtbl.mem env.names name then begin
            env.next_id <- env.next_id + 1;
            name ^ "." ^ (string_of_int env.next_id)
          end else
            name
        in
        assert (not (Hashtbl.mem env.names mangled));
        mangled
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
