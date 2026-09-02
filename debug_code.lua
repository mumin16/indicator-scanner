require("indicators")


function calc()
 
	print("The code will be here!")
  
end

DEBUG_RUN("./debug_data",calc)
--for dir in io.popen([[dir "./data/" /b]]):lines() do DEBUG_RUN("./data/" .. dir,calc) end