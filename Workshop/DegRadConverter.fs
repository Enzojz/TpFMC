namespace TpfMC

open System
open System.Windows.Data

type DegRadConverter() = 
    interface IValueConverter with
        override this.Convert(value, targetType, parameter, culture) = Convert.ToDouble(value.ToString()) / Math.PI * 180.0 :> obj
        override this.ConvertBack(value, targetType, parameter, culture) = failwith "Not implemented"
