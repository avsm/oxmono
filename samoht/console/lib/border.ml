(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type chars = {
  top_left : string;
  top : string;
  top_right : string;
  left : string;
  right : string;
  bottom_left : string;
  bottom : string;
  bottom_right : string;
  cross : string;
  top_cross : string;
  bottom_cross : string;
  left_cross : string;
  right_cross : string;
}

type t = { chars : chars; style : Style.t }

let v ?(style = Style.none) chars = { chars; style }
let chars t = t.chars
let style t = t.style

let none_chars =
  {
    top_left = "";
    top = "";
    top_right = "";
    left = "";
    right = "";
    bottom_left = "";
    bottom = "";
    bottom_right = "";
    cross = "";
    top_cross = "";
    bottom_cross = "";
    left_cross = "";
    right_cross = "";
  }

let ascii_chars =
  {
    top_left = "+";
    top = "-";
    top_right = "+";
    left = "|";
    right = "|";
    bottom_left = "+";
    bottom = "-";
    bottom_right = "+";
    cross = "+";
    top_cross = "+";
    bottom_cross = "+";
    left_cross = "+";
    right_cross = "+";
  }

let single_chars =
  {
    top_left = "┌";
    top = "─";
    top_right = "┐";
    left = "│";
    right = "│";
    bottom_left = "└";
    bottom = "─";
    bottom_right = "┘";
    cross = "┼";
    top_cross = "┬";
    bottom_cross = "┴";
    left_cross = "├";
    right_cross = "┤";
  }

let double_chars =
  {
    top_left = "╔";
    top = "═";
    top_right = "╗";
    left = "║";
    right = "║";
    bottom_left = "╚";
    bottom = "═";
    bottom_right = "╝";
    cross = "╬";
    top_cross = "╦";
    bottom_cross = "╩";
    left_cross = "╠";
    right_cross = "╣";
  }

let rounded_chars =
  {
    top_left = "╭";
    top = "─";
    top_right = "╮";
    left = "│";
    right = "│";
    bottom_left = "╰";
    bottom = "─";
    bottom_right = "╯";
    cross = "┼";
    top_cross = "┬";
    bottom_cross = "┴";
    left_cross = "├";
    right_cross = "┤";
  }

let heavy_chars =
  {
    top_left = "┏";
    top = "━";
    top_right = "┓";
    left = "┃";
    right = "┃";
    bottom_left = "┗";
    bottom = "━";
    bottom_right = "┛";
    cross = "╋";
    top_cross = "┳";
    bottom_cross = "┻";
    left_cross = "┣";
    right_cross = "┫";
  }

let hidden_chars =
  {
    top_left = " ";
    top = " ";
    top_right = " ";
    left = " ";
    right = " ";
    bottom_left = " ";
    bottom = " ";
    bottom_right = " ";
    cross = " ";
    top_cross = " ";
    bottom_cross = " ";
    left_cross = " ";
    right_cross = " ";
  }

let none = v none_chars
let ascii = v ascii_chars
let single = v single_chars
let double = v double_chars
let rounded = v rounded_chars
let heavy = v heavy_chars
let hidden = v hidden_chars
let with_style style border = { border with style }

let equal_chars a b =
  String.equal a.top_left b.top_left
  && String.equal a.top b.top
  && String.equal a.top_right b.top_right
  && String.equal a.left b.left
  && String.equal a.right b.right
  && String.equal a.bottom_left b.bottom_left
  && String.equal a.bottom b.bottom
  && String.equal a.bottom_right b.bottom_right
  && String.equal a.cross b.cross
  && String.equal a.top_cross b.top_cross
  && String.equal a.bottom_cross b.bottom_cross
  && String.equal a.left_cross b.left_cross
  && String.equal a.right_cross b.right_cross

let equal a b = equal_chars a.chars b.chars && Style.equal a.style b.style

let pp ppf t =
  let chars = t.chars in
  Fmt.pf ppf "%s%s%s@." chars.top_left chars.top chars.top_right;
  Fmt.pf ppf "%s %s@." chars.left chars.right;
  Fmt.pf ppf "%s%s%s" chars.bottom_left chars.bottom chars.bottom_right
