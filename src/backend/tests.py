from backend.notifier import Patch
import difflib


patch = '''
@@ -72,10 +72,12 @@ const convertModelsToProps = (
         currentAttempt: childProgress.get('current_attempt'),
       } : null,
     },
-    organization: organization ? {
-      maxAttemptPassQuiz: organization.max_attempt_pass_quiz,
-      blockedHoursPassQuiz: organization.blocked_hours_pass_quiz,
-    } : null,
+    viewer: {
'''[1:-1]

expected = '''
72 72            currentAttempt: childProgress.get('current_attempt'),
73 73          } : null,
74 74        },
75    * -    organization: organization ? {
76      -      maxAttemptPassQuiz: organization.max_attempt_pass_quiz,
77      -      blockedHoursPassQuiz: organization.blocked_hours_pass_quiz,
78      -    } : null,
   75   +    viewer: {
'''[1:-1]


def test_highlight():
    p = Patch(patch)
    highlight = p.highlight(4, context=10)
    assert highlight == expected, '\n'.join(difflib.unified_diff(
        highlight.split('\n'),
        expected.split('\n'),
    ))
