# 通过绘制曲线的直观形式观察平滑函数的效果
import numpy as np 
import matplotlib.pyplot as plt 

dx = 0.00001

def compose(f, g):
    def c(x):
        return f(g(x))
    return c

def repeated(f, n):
    if (n==1):
        return f
    elif (n > 1):
        return compose(f, repeated(f, (n - 1)))
    else:
        raise Exception('n的值不合法。n的值为: {}'.format(n))

def smooth(f):
    def sf(x):
        return (f(x-dx) + f(x) + f(x+dx)) / 3
    return sf

def smooth_n_times(f, n):
    return repeated(smooth(f), n)

x = np.arange(0,  3  * np.pi,  0.1) 
y_sin = np.sin(x) 
y_cos = smooth_n_times(np.sin, 10)(x)  
plt.plot(x, y_sin, 
         x, y_cos, "+") 
plt.show()