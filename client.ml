open Ipaddr
open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js

let ip_input ?on_change value =
  let patt =
    let part = {|(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)|} in
    "^" ^ part ^ "\\." ^ part ^ "\\." ^ part ^ "\\." ^ part ^ "$"
  in
  let inp = Html.(input ~a:[a_pattern patt; a_value (V4.to_string !value)] ()) in
  let elt = To_dom.of_input inp in
  ignore @@ Dom_events.listen elt Dom_html.Event.change (fun _ _ ->
    V4.of_string (Js.to_string elt##.value) |> Result.iter (fun x ->
      value := x;
      Option.value ~default:ignore on_change x
    );
    false
  );
  inp

let range_input ?on_change ?list ~min ~max value =
  let inp =
    Html.(input ~a:(
      a_input_type `Range ::
      a_input_min (`Number min) ::
      a_input_max (`Number max) ::
      a_value (string_of_int !value) ::
      (Option.map a_list list |> Option.to_list)
    ) ())
  in
  let elt = To_dom.of_input inp in
  ignore @@ Dom_events.listen elt Dom_html.Event.change (fun _ _ ->
    int_of_string_opt (Js.to_string elt##.value) |> Option.iter (fun x ->
      value := x;
      Option.value ~default:ignore on_change x
    );
    false
  );
  inp

let output f =
  let inp = Html.(input ~a:[a_readonly ()] ()) in
  let elt = To_dom.of_input inp in
  inp, fun x -> elt##.value := Js.string (f x)

let div =
  let ip = ref (V4.of_string_exn "192.168.1.1") in
  let n = ref 24 in
  let start, update_start = output V4.to_string in
  let end_, update_end = output V4.to_string in
  let info, update =
    let h = Html.txt "" in
    let elt = To_dom.of_pcdata h in
    h, fun () ->
      let pr = V4.Prefix.make !n !ip in
      update_start (V4.Prefix.first pr);
      update_end (V4.Prefix.last pr);
      elt##.data := Js.string ("/" ^ string_of_int !n)
  in
  update ();
  let on_change _ = update () in
  let open Html in
  div [
    datalist ~a:[a_id "cl"] ~children:(`Options (
      [0; 8; 16; 24; 32] |> List.map (fun x ->
        let t = string_of_int x in
        option ~a:[a_value t; a_label t] (txt "")
      )
    )) ();
    label [txt "CIDR: "; ip_input ~on_change ip; info];
    label [txt "Size: "; range_input ~on_change ~list:"cl" ~min:0 ~max:32 n];
    label [txt "Start: "; start];
    label [txt "End: "; end_];
  ]


let _ =
  let open Dom_html in
  Dom_events.listen document Event.domContentLoaded (fun _ _ ->
    let elt = (To_dom.of_div div :> Dom.node Js.t) in
    ignore @@ document##.body##appendChild elt;
    true
  )
