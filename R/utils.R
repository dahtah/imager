iterate <- function(f,k)
    {
        function(x)
            {
                for (ind in 1:k)
                    {
                        x <- f(x)
                    }
                x
            }
    }
