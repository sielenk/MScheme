package MScheme;

public interface Init
{
    String bootstrap = "(begin(define update-null-environment(let()(define(and-func def-env use-env . args)(if(null? args)#t(let((head(car args))(tail(cdr args)))(if(null? tail)head(list 'if head(cons 'and tail)#f)))))(define(or-func def-env use-env . args)(if(null? args)#f(let((head(car args))(tail(cdr args)))(if(null? tail)head(let((id(unique-id)))(list 'let(list(list id head))(list 'if id id(cons 'or tail))))))))(define(make-promise proc)(let((result-ready? #f)(result #f))(lambda()(if result-ready? result(let((x(proc)))(if result-ready? result(begin(set! result-ready? #t)(set! result x)result)))))))(define(delay-func def-env use-env expression)(list make-promise(list 'lambda '()expression)))(define(wrapper func)(lambda(def-env use-env . args)(cons use-env(apply func def-env use-env args))))(lambda(env)(eval(list 'begin(list 'define-syntax 'and(wrapper and-func))(list 'define-syntax 'or(wrapper or-func))(list 'define-syntax 'delay(wrapper delay-func)))env)env)))(update-null-environment(current-environment))(define(reduce func initial args)(if(null? args)initial(let helper([head(car args)][tail(cdr args)])(if(null? tail)head(helper(func head(car tail))(cdr tail))))))(define(reduce-right func initial args)(if(null? args)initial(let helper([head(car args)][tail(cdr args)])(if(null? tail)head(func head(helper(car tail)(cdr tail)))))))(define(fold-right func initial args)(if(null? args)initial(let helper([head(car args)][tail(cdr args)])(func head(if(null? tail)initial(helper(car tail)(cdr tail)))))))(define(fold-left func initial args)(let helper([head initial][tail args])(if(null? tail)head(helper(func head(car tail))(cdr tail)))))(define update-scheme-report-environment(let()(define(primitve-map f l)(fold-right(lambda(x r)(cons(f x)r))'()l))(define(transpose lists)(let loop((rest lists))(if(null?(car rest))'()(cons(primitve-map car rest)(loop(primitve-map cdr rest))))))(define(map func . lists)(if(null?(cdr lists))(primitve-map func(car lists))(primitve-map(lambda(list)(apply func list))(transpose lists))))(define(for-each func . lists)(reverse(apply map func(map reverse lists))))(define(force object)(object))(define(call-with-input-file filename proc)(let*((port(open-input-file filename))(result(proc port)))(close-input-port port)result))(define(call-with-output-file filename proc)(let*((port(open-output-file filename))(result(proc port)))(close-output-port port)result))(define current-input-port 'dummy)(define current-output-port 'dummy)(define with-input-from-file 'dummy)(define with-output-to-file 'dummy)(let([basic-read read][basic-read-char read-char][basic-peek-char peek-char][basic-char-ready? char-ready?][basic-write write][basic-display display][basic-write-char write-char][basic-null-environment null-environment][basic-scheme-report-environment scheme-report-environment][cip initial-input-port][cop initial-output-port])(define current-input-port(lambda()cip))(define current-output-port(lambda()cop))(define(wrap-cip func)(lambda args(if(null? args)(func cip)(apply func args))))(define(wrap-cop func)(lambda(obj . args)(if(null? args)(func obj cop)(apply func obj args))))(define read(wrap-cip basic-read))(define read-char(wrap-cip basic-read-char))(define peek-char(wrap-cip basic-peek-char))(define char-ready?(wrap-cip basic-char-ready?))(define write(wrap-cop basic-write))(define display(wrap-cop basic-display))(define write-char(wrap-cop basic-write-char))(define with-input-from-file(lambda(filename thunk)(call-with-input-file filename(lambda(new-cip)(let([old-cip cip])(dynamic-wind(lambda()(set! cip new-cip))thunk(lambda()(set! cip old-cip))))))))(define with-output-to-file(lambda(filename thunk)(call-with-output-file filename(lambda(new-cop)(let([old-cop cop])(dynamic-wind(lambda()(set! cop new-cop))thunk(lambda()(set! cop old-cop))))))))(define(newline . args)(apply display #\\newline args))(define(make-load env)(lambda(filename)(let([port(open-input-file filename)])(let eval-expr([expr(read port)][result '()])(if(eof-object? expr)result(eval-expr(read port)(eval expr env)))))))(define(null-environment version)(update-null-environment(basic-null-environment version)))(define(scheme-report-environment version)(update-scheme-report-environment(update-null-environment(basic-scheme-report-environment version))))(define(make-definition sym)(list 'define sym(eval sym(current-environment))))(define definition-list(cons 'begin(map make-definition '(map for-each force call-with-input-file call-with-output-file current-input-port current-output-port with-input-from-file with-output-to-file read read-char peek-char char-ready? write display write-char newline null-environment scheme-report-environment))))(lambda(env)(eval definition-list env)(eval(list 'define 'load(make-load env))env)env))))(update-scheme-report-environment(current-environment)))";

    String rep = "(let*([user-env(scheme-report-environment 5)][user-eval(lambda(expr)(eval expr user-env))][user-define(lambda(sym val)(user-eval(list 'define sym val)))][user-load(user-eval 'load)])(define(cadr x)(car(cdr x)))(define(caddr x)(car(cdr(cdr x))))(define(cadddr x)(car(cdr(cdr(cdr x)))))(define error->cause car)(define error->message cadr)(define error->continuation caddr)(define error->retryable cadddr)(define(display-nl . args)(for-each display args)(newline))(define(print-error error)(let([cause(error->cause error)][message(error->message error)][stack(error->continuation error)])(display-nl \"error : \" message)(display \"caused by : \")(write cause)(newline)(display-nl \"--- begin of stack ---\")(display-nl stack)(display-nl \"--- end of stack ---\")))(define(on-error f)(lambda()(let([error(last-error)])(if error(begin(print-error error)(f error))))))(define(try-with-error-handler try-thunk error-handler)(dynamic-wind(lambda()'nop)try-thunk(on-error error-handler)))(define(create-label)(call-with-current-continuation(lambda(k)(define(retry)(k retry))retry)))(define(retry-on-error thunk)(define retry 'dummy)(try-with-error-handler(lambda()(set! retry(create-label))(thunk))(lambda(error)(retry))))(define(REP-read depth prompt quit-thunk)(let*([query(lambda()(if prompt(begin(if(> depth 0)(begin(display #\\[)(display depth)(display #\\])))(prompt)))(read))][input(retry-on-error query)])(if(eof-object? input)(begin(display-nl \"<Ctrl-D>\")(quit-thunk))input)))(define(REP-eval error-REP expr)(call-with-current-continuation(lambda(return)(try-with-error-handler(lambda()(user-eval expr))(lambda(error)(if(error->retryable error)(begin(display-nl \"press Ctrl-D to return to outer REP\")((error->continuation error)(error-REP)))(return '<error>)))))))(define(REP-print expr)(write expr)(newline)expr)(define(REP prompt)(let internal-REP([level 0])(call-with-current-continuation(lambda(quit)(let loop([result #f])(loop(REP-print(REP-eval(lambda()(internal-REP(+ level 1)))(REP-read level prompt(lambda()(quit result)))))))))))(display-nl \"Welcome to MScheme\")(display-nl \"press Ctrl-D to quit\")(REP(lambda()(display \"--> \")))(display-nl \"bye ... \"))";

}
