open StagedCommon
open ToySyntax

let rec stagedToToy
    : StagedCommon.exp -> ToySyntax.expression = function
| IdE id -> EVar id
| ConstE (CInt i) -> EConstant (ConstInt i)
| ConstE (CBool b) -> EConstant (ConstBool b)
| EmpLstE -> EVector []
| AppE (e1, e2) -> EApp (stagedToToy e1, stagedToToy e2)
| AbsE (Abs (id, exp)) -> EFun [[PVar id], stagedToToy exp]
| LetInE (Valbind (id, (FixE (fixid, Abs (fixid', abs)))), body) ->
    if fixid = fixid' then
      ELet (true, [PVar id, stagedToToy abs], stagedToToy body)
    else
      failwith "fixid != fixid'"
| LetInE (Valbind (id, bind), body) ->
    ELet (false, [PVar id, stagedToToy bind], stagedToToy body)
| FixE _ -> failwith "can't translate fix to toy."
| IfE (e1, e2, e3) -> EIf (stagedToToy e1, stagedToToy e2, stagedToToy e3)

(* refs *)
| RefE exp         -> EApp (EVar "ref", stagedToToy exp)
| DerefE exp       -> EApp (EVar "!", stagedToToy exp)
| AssignE (e1, e2) -> EApp (EApp (EVar ":=", stagedToToy e1), stagedToToy e2)

(* staged expressions *)
| ValueE _ | BoxE _ | UnboxE _ | RunE _ | LiftE _ ->
    failwith "staged expressions in stagedToToy."

(* records *)
| EmptyRecE -> ERecord []
| SelectE (exp, id) -> EApp (ERecordAccess id, stagedToToy exp)
| RecUpdE (exp, id, newexp) ->
    EApp (EApp (ERecordUpdate id, stagedToToy exp), stagedToToy newexp)

| SeqE (e1, e2) -> ESeq (stagedToToy e1, stagedToToy e2)

