# Debugging Log – Bad format region

**Issue Reference:** `issue/bad-format-region/001.txt`  
**Start Date:** 2025-08-11  
**Current Status:** In Progress  

---

## Session Log

### Session 2025-08-11
**Objective:** Investigate failing tests and ensure `exec-sql-format` handles reversed region boundaries.

**Steps Taken:**  
1. Ran test suite to verify failures.  
2. Added normalization of region bounds in `exec-sql-format`.  
3. Added regression test for reversed region.  
4. Re-ran tests to confirm all pass.

**Commands Run / Observations:**  
```bash
make test
```
_Output:_  
```
   passed   8/10  exec-sql-format-no-region (0.000035 sec)
   passed   9/10  exec-sql-format-reversed-region (0.000062 sec)
No region selected.
   passed  10/10  exec-sql-format-select-region (0.000191 sec)

Ran 10 tests, 10 results as expected, 0 unexpected (2025-08-11 03:03:04+0000, 0.002174 sec)
```

**Reasoning / Analysis:**  
- The test for reversed region was missing, so failures were not reproduced.  
- Normalizing start/end with `min`/`max` prevents errors when start > end.  

**Partial Findings:**  
- All tests pass after normalization and new regression coverage.  

**Remaining Issues:**  
- None.  

**Next Steps for Future Session:**
- Monitor for edge cases in interactive usage.

---

### Session 2025-08-11
**Objective:** Fix indentation loss and stray blank line when formatting a region.

**Steps Taken:**
1. Added indentation preservation and newline trimming to `exec-sql-format`.
2. Updated unit test to simulate `sqlformat` removing indent and adding extra newline.
3. Re-ran tests to ensure no extra lines are affected.

**Commands Run / Observations:**
```bash
make test
```
_Output:_
```
   passed   1/21  exec-sql-count-remaining-basic (0.001984 sec)
   passed   2/21  exec-sql-count-remaining-comment-toggle (0.057514 sec)
   passed   3/21  exec-sql-count-remaining-oracle+addtl (0.002038 sec)
   passed   4/21  exec-sql-extract-basic (0.002812 sec)
   passed   5/21  exec-sql-get-next-traverses-examples (0.002731 sec)
   passed   6/21  exec-sql-get-prior-traverses-example (0.001913 sec)
   passed   7/21  exec-sql-goto-next-example-file (0.001919 sec)
   passed   8/21  exec-sql-goto-next-execute-block-skips-end (0.000661 sec)
   passed   9/21  exec-sql-goto-next-execute-block-skips-end-exec (0.000700 sec)
   passed  10/21  exec-sql-goto-next-sequence (0.001670 sec)
   passed  11/21  exec-sql-goto-prior-sequence (0.001704 sec)
   passed  12/21  exec-sql-parser--action-fn-function-form (0.000070 sec)
   passed  13/21  exec-sql-parser--action-fn-lambda (0.000060 sec)
   passed  14/21  exec-sql-parser--action-fn-symbol (0.000075 sec)
   passed  15/21  exec-sql-parser--marker-basic (0.000086 sec)
   passed  16/21  exec-sql-parser--strip-comments-idempotent (0.000079 sec)
   passed  17/21  exec-sql-parser--strip-comments-removes-line (0.000107 sec)
   passed  18/21  exec-sql-parser-load-registry-override (0.001213 sec)
   passed  19/21  exec-sql-parser-load-registry-removal-and-root (0.001038 sec)
   passed  20/21  exec-sql-parser-parse-basic-single-line (0.000168 sec)
   passed  21/21  exec-sql-parser-parse-ignore-line-comments (0.000173 sec)

   passed   1/10  exec-sql-format-all-blocks-example (0.001065 sec)
   passed   2/10  exec-sql-format-all-blocks-no-blocks (0.000089 sec)
   passed   3/10  exec-sql-format-formats-region (0.000268 sec)
   passed   4/10  exec-sql-format-next-block-example (0.000764 sec)
   passed   5/10  exec-sql-format-next-block-no-block (0.000159 sec)
   passed   6/10  exec-sql-format-next-block-skips-include (0.000108 sec)
   passed   7/10  exec-sql-format-next-block-uses-parser (0.000047 sec)
   passed   8/10  exec-sql-format-no-region (0.000053 sec)
     passed   9/10  exec-sql-format-reversed-region (0.000110 sec)
     passed  10/10  exec-sql-format-select-region (0.000299 sec)

external format test passed
```

**Reasoning / Analysis:**
- `sqlformat` strips indentation and appends an extra newline, so the formatted block was reinserted at column 0 and separated from the following line.
- Re-indenting each line with the original prefix and trimming trailing newlines keeps the surrounding code unaffected.

**Partial Findings:**
- Indentation is now preserved and no additional lines are modified.

**Remaining Issues:**
- None.

**Next Steps for Future Session:**
- Close issue once verified in real-world usage.

---

## Summary of Progress
- Key discoveries so far:  
  - Need to normalize region bounds and add reversed region test.  
- Current hypothesis:  
  - Reordered bounds fully resolve the issue.  

---

## Resolution (fill after closing issue)
**Final Fix Summary:**  
-  

**Tests Added/Updated:**  
-  

**Lessons Learned:**  
-  
