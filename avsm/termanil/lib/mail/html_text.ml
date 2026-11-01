(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
let render html = Termanil_model.safe_text (Jmap_eio.Mail_text.render html)
