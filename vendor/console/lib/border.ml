(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
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
  vertical : string;
  horizontal : string;
  cross : string;
  top_cross : string;
  bottom_cross : string;
  left_cross : string;
  right_cross : string;
}

type t = chars Inked.t

let clean = Console_text.sanitize ~keep_newlines:false

let v ?style c =
  Inked.v ?style
    {
      top_left = clean c.top_left;
      top = clean c.top;
      top_right = clean c.top_right;
      left = clean c.left;
      right = clean c.right;
      bottom_left = clean c.bottom_left;
      bottom = clean c.bottom;
      bottom_right = clean c.bottom_right;
      vertical = clean c.vertical;
      horizontal = clean c.horizontal;
      cross = clean c.cross;
      top_cross = clean c.top_cross;
      bottom_cross = clean c.bottom_cross;
      left_cross = clean c.left_cross;
      right_cross = clean c.right_cross;
    }

let chars = Inked.get
let style = Inked.style

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
    vertical = "";
    horizontal = "";
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
    vertical = "|";
    horizontal = "-";
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
    vertical = "│";
    horizontal = "─";
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
    vertical = "║";
    horizontal = "═";
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
    vertical = "│";
    horizontal = "─";
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
    vertical = "┃";
    horizontal = "━";
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
    vertical = " ";
    horizontal = " ";
    cross = " ";
    top_cross = " ";
    bottom_cross = " ";
    left_cross = " ";
    right_cross = " ";
  }

let dos_chars =
  {
    double_chars with
    vertical = "│";
    horizontal = "─";
    cross = "┼";
    top_cross = "╤";
    bottom_cross = "╧";
    left_cross = "╟";
    right_cross = "╢";
  }

let simple_chars = { none_chars with horizontal = "─" }
let none = v none_chars
let simple = v simple_chars
let ascii = v ascii_chars
let single = v single_chars
let double = v double_chars
let dos = v dos_chars
let rounded = v rounded_chars
let heavy = v heavy_chars
let hidden = v hidden_chars
let with_style = Inked.with_style

let pp ppf t =
  let chars = Inked.get t in
  Fmt.pf ppf "%s%s%s@." chars.top_left chars.top chars.top_right;
  Fmt.pf ppf "%s %s@." chars.left chars.right;
  Fmt.pf ppf "%s%s%s" chars.bottom_left chars.bottom chars.bottom_right
