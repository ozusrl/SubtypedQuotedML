module StagedIdGen (P : sig end) = struct

  let last_name = ref 0
  let new_name _ =
    let int_to_name i =
      let rec mkname i res =
	if i < 26 then
	  Char.chr (97+i) :: res
	else
	  mkname (i/26-1) (Char.chr (97 + i mod 26) :: res)
      in
      let name_lst = mkname i [] in
      let len = List.length name_lst in
      let str = String.make len ' ' in
      for j = 0 to (len-1) do
	str.[j] <- List.nth name_lst j
      done;
      str
      in
      let r = int_to_name !last_name in
      last_name := !last_name + 1;
      r

end
