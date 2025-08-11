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
