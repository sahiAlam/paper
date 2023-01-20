c      programe newton raphson vw
       implicit none
       real:: error, delta, x0, x1, func, dfunc
       integer:: i, n
       
          write(*,*) 'give the values of error'
          read(*,*) error
       write(*,*) 'give the value of delta'
       read(*,*) delta
       write(*,*) 'give the value of iterations'
       read(*,*) n
          write(*,*) 'give the value of x0'
       read(*,*) x0
       
       do i=1,n
       if( (abs(dfunc(x0))) .LE. delta) exit
       x1=x0-(func(x0)/dfunc(x0))
       if ((abs( (x1-x0)/x1)) .LT. error) exit
         x0=x1
         
             write(*,*) 'x0' , x0
          end do
              write(*,*) 'x1=' , x1
              write(*,*) 'i=' , i
           pause
           stop
           end
           
           function func(x)
           real:: func
           real::x
           func=(x**3)-0.3664*(x**2)+0.03802*x-0.001201
           end function func
           function dfunc(x)
            real:: dfunc
            real::x
            dfunc=3*x**2-0.7382*x+0.03802
            end function dfunc


