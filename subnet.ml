let log2 x = log x /. log 2.

(* TODO functorize over IP version? use a GADT? *)

(** [bits n] returns the minimal IPv4 prefix length for [n] hosts. *)
let bits hosts = 32 - int_of_float (ceil (log2 (float (hosts + 2))))

let flsm net l =
  let n = bits (List.fold_left (fun acc (_, x) -> max acc x) 0 l) in
  (* FIXME check n against net! *)
  l |> ListLabels.fold_left ~init:(net, []) ~f:(fun (start, acc) (name, hosts) ->
    let open Ipaddr.V4 in
    let prefix = Prefix.make n start in
    Result.get_ok (succ (Prefix.broadcast prefix)), (name, hosts, prefix) :: acc
  ) |>
  snd

let vlsm net subnets =
  List.sort (fun (_, a) (_, b) -> compare b a (* descending *)) subnets |>
  ListLabels.fold_left ~init:(net, []) ~f:(fun (start, acc) (name, hosts) ->
    let open Ipaddr.V4 in
    let bits = bits hosts in
    (* FIXME check bits against net! *)
    let prefix = Prefix.make bits start in
    Result.get_ok (succ (Prefix.broadcast prefix)), (name, hosts, prefix) :: acc
  ) |>
  snd

let available p = 1 lsl (32 - Ipaddr.V4.Prefix.bits p) - 2

let efficiency l =
  match
    List.map (fun (_, hosts, p) -> hosts, available p) l |>
    List.fold_left (fun (n, d) (n', d') -> n + n', d + d') (0, 0)
  with
  | _, 0 -> 1.
  | n, d -> float n /. float d

(* TODO rewrite https://subnettingpractice.com/vlsm.html *)
let () =
  flsm (Ipaddr.V4.of_string_exn "192.168.0.0") ["a", 120; "b", 8; "c", 8] |>
  List.iter (fun (name, hosts, p) ->
    let open Ipaddr.V4 in
    let available = available p in
    Format.printf "%s: %a-%a, mask %a, broadcast %a, used %d/%d (%d%%)\n"
      name
      pp (Prefix.first p)
      pp (Prefix.last p)
      pp (Prefix.netmask p)
      pp (Prefix.broadcast p)
      hosts
      available
      (100 * hosts / available)
  )
