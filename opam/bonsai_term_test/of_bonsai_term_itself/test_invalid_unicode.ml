open! Core
open Bonsai_term
open Bonsai_term_test

module Raises_or_not = struct
  type t =
    | Raises
    | Ok
  [@@deriving sexp_of]
end

let raises_or_not f =
  match Option.try_with f with
  | Some _ -> Raises_or_not.Ok
  | None -> Raises_or_not.Raises
;;

let%expect_test "NOTE we do not crash on all characters" =
  let () =
    Expectable.print
    @@ List.map Char.all ~f:(fun c ->
      (* NOTE: Expectable does not handle all of the characters so we print out the int
         representation. *)
      let code = Char.to_int c in
      let string = String.of_char_list [ c ] in
      let force_view view =
        (* setting the background and foreground colors will fully evaluate the view.t,
           triggering an exception if the string is invalid utf8 *)
        let black = Attr.Color.rgb ~r:0 ~g:0 ~b:0 in
        View.with_colors ~fg:black ~bg:black view
      in
      let raises_or_ok = raises_or_not (fun () -> force_view (View.text string)) in
      let as_string =
        match String.Utf8.is_valid string with
        | false -> None
        | true -> Some string
      in
      let as_sexp = Sexp.to_string [%sexp (as_string : string option)] in
      [%message (code : int) (raises_or_ok : Raises_or_not.t) (as_sexp : string)])
  in
  [%expect
    {|
    ┌──────┬──────────────┬──────────┐
    │ code │ raises_or_ok │ as_sexp  │
    ├──────┼──────────────┼──────────┤
    │   0  │ Ok           │ ("\000") │
    │   1  │ Ok           │ ("\001") │
    │   2  │ Ok           │ ("\002") │
    │   3  │ Ok           │ ("\003") │
    │   4  │ Ok           │ ("\004") │
    │   5  │ Ok           │ ("\005") │
    │   6  │ Ok           │ ("\006") │
    │   7  │ Ok           │ ("\007") │
    │   8  │ Ok           │ ("\b")   │
    │   9  │ Ok           │ ("\t")   │
    │  10  │ Ok           │ ("\n")   │
    │  11  │ Ok           │ ("\011") │
    │  12  │ Ok           │ ("\012") │
    │  13  │ Ok           │ ("\r")   │
    │  14  │ Ok           │ ("\014") │
    │  15  │ Ok           │ ("\015") │
    │  16  │ Ok           │ ("\016") │
    │  17  │ Ok           │ ("\017") │
    │  18  │ Ok           │ ("\018") │
    │  19  │ Ok           │ ("\019") │
    │  20  │ Ok           │ ("\020") │
    │  21  │ Ok           │ ("\021") │
    │  22  │ Ok           │ ("\022") │
    │  23  │ Ok           │ ("\023") │
    │  24  │ Ok           │ ("\024") │
    │  25  │ Ok           │ ("\025") │
    │  26  │ Ok           │ ("\026") │
    │  27  │ Ok           │ ("\027") │
    │  28  │ Ok           │ ("\028") │
    │  29  │ Ok           │ ("\029") │
    │  30  │ Ok           │ ("\030") │
    │  31  │ Ok           │ ("\031") │
    │  32  │ Ok           │ (" ")    │
    │  33  │ Ok           │ (!)      │
    │  34  │ Ok           │ ("\"")   │
    │  35  │ Ok           │ (#)      │
    │  36  │ Ok           │ ($)      │
    │  37  │ Ok           │ (%)      │
    │  38  │ Ok           │ (&)      │
    │  39  │ Ok           │ (')      │
    │  40  │ Ok           │ ("(")    │
    │  41  │ Ok           │ (")")    │
    │  42  │ Ok           │ (*)      │
    │  43  │ Ok           │ (+)      │
    │  44  │ Ok           │ (,)      │
    │  45  │ Ok           │ (-)      │
    │  46  │ Ok           │ (.)      │
    │  47  │ Ok           │ (/)      │
    │  48  │ Ok           │ (0)      │
    │  49  │ Ok           │ (1)      │
    │  50  │ Ok           │ (2)      │
    │  51  │ Ok           │ (3)      │
    │  52  │ Ok           │ (4)      │
    │  53  │ Ok           │ (5)      │
    │  54  │ Ok           │ (6)      │
    │  55  │ Ok           │ (7)      │
    │  56  │ Ok           │ (8)      │
    │  57  │ Ok           │ (9)      │
    │  58  │ Ok           │ (:)      │
    │  59  │ Ok           │ (";")    │
    │  60  │ Ok           │ (<)      │
    │  61  │ Ok           │ (=)      │
    │  62  │ Ok           │ (>)      │
    │  63  │ Ok           │ (?)      │
    │  64  │ Ok           │ (@)      │
    │  65  │ Ok           │ (A)      │
    │  66  │ Ok           │ (B)      │
    │  67  │ Ok           │ (C)      │
    │  68  │ Ok           │ (D)      │
    │  69  │ Ok           │ (E)      │
    │  70  │ Ok           │ (F)      │
    │  71  │ Ok           │ (G)      │
    │  72  │ Ok           │ (H)      │
    │  73  │ Ok           │ (I)      │
    │  74  │ Ok           │ (J)      │
    │  75  │ Ok           │ (K)      │
    │  76  │ Ok           │ (L)      │
    │  77  │ Ok           │ (M)      │
    │  78  │ Ok           │ (N)      │
    │  79  │ Ok           │ (O)      │
    │  80  │ Ok           │ (P)      │
    │  81  │ Ok           │ (Q)      │
    │  82  │ Ok           │ (R)      │
    │  83  │ Ok           │ (S)      │
    │  84  │ Ok           │ (T)      │
    │  85  │ Ok           │ (U)      │
    │  86  │ Ok           │ (V)      │
    │  87  │ Ok           │ (W)      │
    │  88  │ Ok           │ (X)      │
    │  89  │ Ok           │ (Y)      │
    │  90  │ Ok           │ (Z)      │
    │  91  │ Ok           │ ([)      │
    │  92  │ Ok           │ ("\\")   │
    │  93  │ Ok           │ (])      │
    │  94  │ Ok           │ (^)      │
    │  95  │ Ok           │ (_)      │
    │  96  │ Ok           │ (`)      │
    │  97  │ Ok           │ (a)      │
    │  98  │ Ok           │ (b)      │
    │  99  │ Ok           │ (c)      │
    │ 100  │ Ok           │ (d)      │
    │ 101  │ Ok           │ (e)      │
    │ 102  │ Ok           │ (f)      │
    │ 103  │ Ok           │ (g)      │
    │ 104  │ Ok           │ (h)      │
    │ 105  │ Ok           │ (i)      │
    │ 106  │ Ok           │ (j)      │
    │ 107  │ Ok           │ (k)      │
    │ 108  │ Ok           │ (l)      │
    │ 109  │ Ok           │ (m)      │
    │ 110  │ Ok           │ (n)      │
    │ 111  │ Ok           │ (o)      │
    │ 112  │ Ok           │ (p)      │
    │ 113  │ Ok           │ (q)      │
    │ 114  │ Ok           │ (r)      │
    │ 115  │ Ok           │ (s)      │
    │ 116  │ Ok           │ (t)      │
    │ 117  │ Ok           │ (u)      │
    │ 118  │ Ok           │ (v)      │
    │ 119  │ Ok           │ (w)      │
    │ 120  │ Ok           │ (x)      │
    │ 121  │ Ok           │ (y)      │
    │ 122  │ Ok           │ (z)      │
    │ 123  │ Ok           │ ({)      │
    │ 124  │ Ok           │ (|)      │
    │ 125  │ Ok           │ (})      │
    │ 126  │ Ok           │ (~)      │
    │ 127  │ Ok           │ ("\127") │
    │ 128  │ Ok           │ ()       │
    │ 129  │ Ok           │ ()       │
    │ 130  │ Ok           │ ()       │
    │ 131  │ Ok           │ ()       │
    │ 132  │ Ok           │ ()       │
    │ 133  │ Ok           │ ()       │
    │ 134  │ Ok           │ ()       │
    │ 135  │ Ok           │ ()       │
    │ 136  │ Ok           │ ()       │
    │ 137  │ Ok           │ ()       │
    │ 138  │ Ok           │ ()       │
    │ 139  │ Ok           │ ()       │
    │ 140  │ Ok           │ ()       │
    │ 141  │ Ok           │ ()       │
    │ 142  │ Ok           │ ()       │
    │ 143  │ Ok           │ ()       │
    │ 144  │ Ok           │ ()       │
    │ 145  │ Ok           │ ()       │
    │ 146  │ Ok           │ ()       │
    │ 147  │ Ok           │ ()       │
    │ 148  │ Ok           │ ()       │
    │ 149  │ Ok           │ ()       │
    │ 150  │ Ok           │ ()       │
    │ 151  │ Ok           │ ()       │
    │ 152  │ Ok           │ ()       │
    │ 153  │ Ok           │ ()       │
    │ 154  │ Ok           │ ()       │
    │ 155  │ Ok           │ ()       │
    │ 156  │ Ok           │ ()       │
    │ 157  │ Ok           │ ()       │
    │ 158  │ Ok           │ ()       │
    │ 159  │ Ok           │ ()       │
    │ 160  │ Ok           │ ()       │
    │ 161  │ Ok           │ ()       │
    │ 162  │ Ok           │ ()       │
    │ 163  │ Ok           │ ()       │
    │ 164  │ Ok           │ ()       │
    │ 165  │ Ok           │ ()       │
    │ 166  │ Ok           │ ()       │
    │ 167  │ Ok           │ ()       │
    │ 168  │ Ok           │ ()       │
    │ 169  │ Ok           │ ()       │
    │ 170  │ Ok           │ ()       │
    │ 171  │ Ok           │ ()       │
    │ 172  │ Ok           │ ()       │
    │ 173  │ Ok           │ ()       │
    │ 174  │ Ok           │ ()       │
    │ 175  │ Ok           │ ()       │
    │ 176  │ Ok           │ ()       │
    │ 177  │ Ok           │ ()       │
    │ 178  │ Ok           │ ()       │
    │ 179  │ Ok           │ ()       │
    │ 180  │ Ok           │ ()       │
    │ 181  │ Ok           │ ()       │
    │ 182  │ Ok           │ ()       │
    │ 183  │ Ok           │ ()       │
    │ 184  │ Ok           │ ()       │
    │ 185  │ Ok           │ ()       │
    │ 186  │ Ok           │ ()       │
    │ 187  │ Ok           │ ()       │
    │ 188  │ Ok           │ ()       │
    │ 189  │ Ok           │ ()       │
    │ 190  │ Ok           │ ()       │
    │ 191  │ Ok           │ ()       │
    │ 192  │ Ok           │ ()       │
    │ 193  │ Ok           │ ()       │
    │ 194  │ Ok           │ ()       │
    │ 195  │ Ok           │ ()       │
    │ 196  │ Ok           │ ()       │
    │ 197  │ Ok           │ ()       │
    │ 198  │ Ok           │ ()       │
    │ 199  │ Ok           │ ()       │
    │ 200  │ Ok           │ ()       │
    │ 201  │ Ok           │ ()       │
    │ 202  │ Ok           │ ()       │
    │ 203  │ Ok           │ ()       │
    │ 204  │ Ok           │ ()       │
    │ 205  │ Ok           │ ()       │
    │ 206  │ Ok           │ ()       │
    │ 207  │ Ok           │ ()       │
    │ 208  │ Ok           │ ()       │
    │ 209  │ Ok           │ ()       │
    │ 210  │ Ok           │ ()       │
    │ 211  │ Ok           │ ()       │
    │ 212  │ Ok           │ ()       │
    │ 213  │ Ok           │ ()       │
    │ 214  │ Ok           │ ()       │
    │ 215  │ Ok           │ ()       │
    │ 216  │ Ok           │ ()       │
    │ 217  │ Ok           │ ()       │
    │ 218  │ Ok           │ ()       │
    │ 219  │ Ok           │ ()       │
    │ 220  │ Ok           │ ()       │
    │ 221  │ Ok           │ ()       │
    │ 222  │ Ok           │ ()       │
    │ 223  │ Ok           │ ()       │
    │ 224  │ Ok           │ ()       │
    │ 225  │ Ok           │ ()       │
    │ 226  │ Ok           │ ()       │
    │ 227  │ Ok           │ ()       │
    │ 228  │ Ok           │ ()       │
    │ 229  │ Ok           │ ()       │
    │ 230  │ Ok           │ ()       │
    │ 231  │ Ok           │ ()       │
    │ 232  │ Ok           │ ()       │
    │ 233  │ Ok           │ ()       │
    │ 234  │ Ok           │ ()       │
    │ 235  │ Ok           │ ()       │
    │ 236  │ Ok           │ ()       │
    │ 237  │ Ok           │ ()       │
    │ 238  │ Ok           │ ()       │
    │ 239  │ Ok           │ ()       │
    │ 240  │ Ok           │ ()       │
    │ 241  │ Ok           │ ()       │
    │ 242  │ Ok           │ ()       │
    │ 243  │ Ok           │ ()       │
    │ 244  │ Ok           │ ()       │
    │ 245  │ Ok           │ ()       │
    │ 246  │ Ok           │ ()       │
    │ 247  │ Ok           │ ()       │
    │ 248  │ Ok           │ ()       │
    │ 249  │ Ok           │ ()       │
    │ 250  │ Ok           │ ()       │
    │ 251  │ Ok           │ ()       │
    │ 252  │ Ok           │ ()       │
    │ 253  │ Ok           │ ()       │
    │ 254  │ Ok           │ ()       │
    │ 255  │ Ok           │ ()       │
    └──────┴──────────────┴──────────┘
    |}]
;;

let%expect_test "Invalid unicode character" =
  let view =
    View.text
      ("this is bad:"
       ^ Char.to_string (Char.of_int_exn 128)
       ^ "something else that is valid")
  in
  let handle =
    create_handle
      ~initial_dimensions:{ width = 80; height = 3 }
      (fun ~dimensions:_ (local_ _graph) ->
         let view = Bonsai.return view in
         let handler = Bonsai.return (fun _ -> Effect.Ignore) in
         ~view, ~handler)
  in
  Bonsai_test.Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │this is bad:�something else that is valid                                       │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "an expect test of a bonsai term app that prints out every character" =
  let view =
    View.vcat
    @@
    let%map.List char = Char.all in
    let int = Char.to_int char in
    View.text [%string "%{int#Int}: %{char#Char}"]
  in
  let handle =
    create_handle
      ~initial_dimensions:{ width = 80; height = 280 }
      (fun ~dimensions:_ (local_ _graph) ->
         let view = Bonsai.return view in
         let handler = Bonsai.return (fun _ -> Effect.Ignore) in
         ~view, ~handler)
  in
  (* NOTE: Importantly we never crash!! *)
  Bonsai_test.Handle.show handle;
  [%expect
    {|
    ┌────────────────────────────────────────────────────────────────────────────────┐
    │0: \000                                                                         │
    │1: \001                                                                         │
    │2: \002                                                                         │
    │3: \003                                                                         │
    │4: \004                                                                         │
    │5: \005                                                                         │
    │6: \006                                                                         │
    │7: \007                                                                         │
    │8: \b                                                                           │
    │9: \t                                                                           │
    │10: \n                                                                          │
    │11: \011                                                                        │
    │12: \012                                                                        │
    │13: \r                                                                          │
    │14: \014                                                                        │
    │15: \015                                                                        │
    │16: \016                                                                        │
    │17: \017                                                                        │
    │18: \018                                                                        │
    │19: \019                                                                        │
    │20: \020                                                                        │
    │21: \021                                                                        │
    │22: \022                                                                        │
    │23: \023                                                                        │
    │24: \024                                                                        │
    │25: \025                                                                        │
    │26: \026                                                                        │
    │27: \027                                                                        │
    │28: \028                                                                        │
    │29: \029                                                                        │
    │30: \030                                                                        │
    │31: \031                                                                        │
    │32:                                                                             │
    │33: !                                                                           │
    │34: "                                                                           │
    │35: #                                                                           │
    │36: $                                                                           │
    │37: %                                                                           │
    │38: &                                                                           │
    │39: '                                                                           │
    │40: (                                                                           │
    │41: )                                                                           │
    │42: *                                                                           │
    │43: +                                                                           │
    │44: ,                                                                           │
    │45: -                                                                           │
    │46: .                                                                           │
    │47: /                                                                           │
    │48: 0                                                                           │
    │49: 1                                                                           │
    │50: 2                                                                           │
    │51: 3                                                                           │
    │52: 4                                                                           │
    │53: 5                                                                           │
    │54: 6                                                                           │
    │55: 7                                                                           │
    │56: 8                                                                           │
    │57: 9                                                                           │
    │58: :                                                                           │
    │59: ;                                                                           │
    │60: <                                                                           │
    │61: =                                                                           │
    │62: >                                                                           │
    │63: ?                                                                           │
    │64: @                                                                           │
    │65: A                                                                           │
    │66: B                                                                           │
    │67: C                                                                           │
    │68: D                                                                           │
    │69: E                                                                           │
    │70: F                                                                           │
    │71: G                                                                           │
    │72: H                                                                           │
    │73: I                                                                           │
    │74: J                                                                           │
    │75: K                                                                           │
    │76: L                                                                           │
    │77: M                                                                           │
    │78: N                                                                           │
    │79: O                                                                           │
    │80: P                                                                           │
    │81: Q                                                                           │
    │82: R                                                                           │
    │83: S                                                                           │
    │84: T                                                                           │
    │85: U                                                                           │
    │86: V                                                                           │
    │87: W                                                                           │
    │88: X                                                                           │
    │89: Y                                                                           │
    │90: Z                                                                           │
    │91: [                                                                           │
    │92: \                                                                           │
    │93: ]                                                                           │
    │94: ^                                                                           │
    │95: _                                                                           │
    │96: `                                                                           │
    │97: a                                                                           │
    │98: b                                                                           │
    │99: c                                                                           │
    │100: d                                                                          │
    │101: e                                                                          │
    │102: f                                                                          │
    │103: g                                                                          │
    │104: h                                                                          │
    │105: i                                                                          │
    │106: j                                                                          │
    │107: k                                                                          │
    │108: l                                                                          │
    │109: m                                                                          │
    │110: n                                                                          │
    │111: o                                                                          │
    │112: p                                                                          │
    │113: q                                                                          │
    │114: r                                                                          │
    │115: s                                                                          │
    │116: t                                                                          │
    │117: u                                                                          │
    │118: v                                                                          │
    │119: w                                                                          │
    │120: x                                                                          │
    │121: y                                                                          │
    │122: z                                                                          │
    │123: {                                                                          │
    │124: |                                                                          │
    │125: }                                                                          │
    │126: ~                                                                          │
    │127: \127                                                                       │
    │128: �                                                                          │
    │129: �                                                                          │
    │130: �                                                                          │
    │131: �                                                                          │
    │132: �                                                                          │
    │133: �                                                                          │
    │134: �                                                                          │
    │135: �                                                                          │
    │136: �                                                                          │
    │137: �                                                                          │
    │138: �                                                                          │
    │139: �                                                                          │
    │140: �                                                                          │
    │141: �                                                                          │
    │142: �                                                                          │
    │143: �                                                                          │
    │144: �                                                                          │
    │145: �                                                                          │
    │146: �                                                                          │
    │147: �                                                                          │
    │148: �                                                                          │
    │149: �                                                                          │
    │150: �                                                                          │
    │151: �                                                                          │
    │152: �                                                                          │
    │153: �                                                                          │
    │154: �                                                                          │
    │155: �                                                                          │
    │156: �                                                                          │
    │157: �                                                                          │
    │158: �                                                                          │
    │159: �                                                                          │
    │160: �                                                                          │
    │161: �                                                                          │
    │162: �                                                                          │
    │163: �                                                                          │
    │164: �                                                                          │
    │165: �                                                                          │
    │166: �                                                                          │
    │167: �                                                                          │
    │168: �                                                                          │
    │169: �                                                                          │
    │170: �                                                                          │
    │171: �                                                                          │
    │172: �                                                                          │
    │173: �                                                                          │
    │174: �                                                                          │
    │175: �                                                                          │
    │176: �                                                                          │
    │177: �                                                                          │
    │178: �                                                                          │
    │179: �                                                                          │
    │180: �                                                                          │
    │181: �                                                                          │
    │182: �                                                                          │
    │183: �                                                                          │
    │184: �                                                                          │
    │185: �                                                                          │
    │186: �                                                                          │
    │187: �                                                                          │
    │188: �                                                                          │
    │189: �                                                                          │
    │190: �                                                                          │
    │191: �                                                                          │
    │192: �                                                                          │
    │193: �                                                                          │
    │194: �                                                                          │
    │195: �                                                                          │
    │196: �                                                                          │
    │197: �                                                                          │
    │198: �                                                                          │
    │199: �                                                                          │
    │200: �                                                                          │
    │201: �                                                                          │
    │202: �                                                                          │
    │203: �                                                                          │
    │204: �                                                                          │
    │205: �                                                                          │
    │206: �                                                                          │
    │207: �                                                                          │
    │208: �                                                                          │
    │209: �                                                                          │
    │210: �                                                                          │
    │211: �                                                                          │
    │212: �                                                                          │
    │213: �                                                                          │
    │214: �                                                                          │
    │215: �                                                                          │
    │216: �                                                                          │
    │217: �                                                                          │
    │218: �                                                                          │
    │219: �                                                                          │
    │220: �                                                                          │
    │221: �                                                                          │
    │222: �                                                                          │
    │223: �                                                                          │
    │224: �                                                                          │
    │225: �                                                                          │
    │226: �                                                                          │
    │227: �                                                                          │
    │228: �                                                                          │
    │229: �                                                                          │
    │230: �                                                                          │
    │231: �                                                                          │
    │232: �                                                                          │
    │233: �                                                                          │
    │234: �                                                                          │
    │235: �                                                                          │
    │236: �                                                                          │
    │237: �                                                                          │
    │238: �                                                                          │
    │239: �                                                                          │
    │240: �                                                                          │
    │241: �                                                                          │
    │242: �                                                                          │
    │243: �                                                                          │
    │244: �                                                                          │
    │245: �                                                                          │
    │246: �                                                                          │
    │247: �                                                                          │
    │248: �                                                                          │
    │249: �                                                                          │
    │250: �                                                                          │
    │251: �                                                                          │
    │252: �                                                                          │
    │253: �                                                                          │
    │254: �                                                                          │
    │255: �                                                                          │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    │                                                                                │
    └────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "an expect test of a bonsai term app that prints out every character" =
  let%quick_test prop (string : string) =
    (* NOTE: we never raise on quickcheck generated strings! *)
    let view = View.text string in
    let handle =
      create_handle
        ~initial_dimensions:{ width = 80; height = 280 }
        (fun ~dimensions:_ (local_ _graph) ->
           let view = Bonsai.return view in
           let handler = Bonsai.return (fun _ -> Effect.Ignore) in
           ~view, ~handler)
    in
    Bonsai_test.Handle.show handle;
    let _ : _ = Expect_test_helpers_core.expect_test_output () in
    ()
      [@@remember_failures {|"\026"|}]
  in
  [%expect {| |}]
;;
