---------------------------------------------------------------
-- 指定某些app 打开后的大小布局
local laptopScreen = hs.screen.allScreens()[1]:name()
local windowLayout = {
   -- 第4 5 6 个参数都是用来描述窗口大小位置的，这三个参数只能设置其中一个
   -- 区别是第4个 是按百分比排位置如 hs.layout.left50={x=0, y=0, w=0.5, h=1}
   -- 是第5个是按象素指定大小 （此时不考虑menu与dock的）
   -- 是第6个是按象素指定大小 （此时考虑menu与dock的）
   -- {"Safari",  nil,          laptopScreen, hs.layout.maximized,   nil , nil},
   -- {"Safari",  nil,          laptopScreen, hs.layout.left50,    nil, nil},
   -- {"邮件",    nil,          laptopScreen, hs.layout.right50,   nil, nil},
   -- {"Emacs",  nil,     laptopScreen, hs.layout.maximized, nil, nil},
   -- {"Emacs",  nil,     laptopScreen, nil, nil, hs.geometry.rect(100, 100, 800, 480)},
   -- {"iTunes",  "MiniPlayer", laptopScreen, nil, nil, hs.geometry.rect(0, -48, 400, 48)},
}
hs.layout.apply(windowLayout)
