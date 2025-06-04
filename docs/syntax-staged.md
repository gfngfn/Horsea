# Syntax of the Staged Language

## Term expressions

```
〈e〉 ::=
  (〈e〉) ｜ 〈c〉 ｜ 〈x〉 ｜ 〈longx〉
｜ fun (〈x〉 : 〈T〉) -> 〈e〉 ｜ 〈e〉 〈e〉
｜ fun {〈x〉 : 〈T〉} -> 〈e〉 ｜ 〈e〉 {〈e〉}
｜ let 〈x〉 = 〈e〉 in 〈e〉 ｜ 〈e〉 as 〈T〉
｜ (〈e〉, 〈e〉) ｜ let (〈x〉, 〈x〉) = 〈e〉 in 〈e〉
｜ &〈e〉 ｜  ~〈e〉
```

### Constants

```
〈c〉 ::=  () ｜ true ｜ false ｜ 〈cn〉 ｜ 〈cfl〉 ｜ 〈cstr〉 ｜ 〈cvec〉 ｜ 〈cmat〉
```

* Base constants:
  - Unit value: `()`.
  - Booleans: `true` or `false`.
  - Integers `〈cn〉`: `0` or a non-empty finite sequence of digits that does not start with `0` (e.g. `42`).
  - Floating-point numbers `〈cfl〉`: a sequence consitituted by a concatenation of (1) integer literals, (2) the dot `.`, and (3) a non-empty finite sequence of digits (e.g. `42.5`).
  - Strings `〈cstr〉`: any finite sequence of characters enclosed by two `"`s, where `"` and `\` are represented by `\"` and `\\`, respectively.
* Tensor constants:
  - Vertical vectors `〈cvec〉`: a finite sequence of integer literals separated by `;` enclosed by `[|` and `|]` (e.g. `[| 3; 1; 4 |]`).
  - Matrices `〈cmat〉`: a finite sequence of rows separated by `;` enclosed by `[#` and `#]`, where each row is a finite sequence of integer literals separated by `,` (e.g. `[# 4, 2; 5, 5 #]`).
