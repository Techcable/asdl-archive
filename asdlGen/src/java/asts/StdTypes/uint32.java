/* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_import=asts.StdPrims
  --line_width=74
  --no_action=false
  --output_directory=../asdl/tests
  --view=Java
  --xml_pickler=false
  */
package asts.StdTypes;
import asts.StdPrims.*;
public final class uint32 { public int int1;
    
    public uint32(int int1) {
        this.int1 = int1;
        
    }
    public final static void write(asts.StdTypes.uint32 x, java.io.OutputStream s) {
         g.write_int(x.int1, s);
     }
    public final static asts.StdTypes.uint32 read(java.io.InputStream s) {
         asts.StdTypes.uint32 t1;
         int int1;
         int1 = g.read_int(s);
         t1 = new asts.StdTypes.uint32(int1);
         return t1;
     }
    public final void accept(asts.StdTypes.Visitor v) {
         v.visit_uint32(this);
     }
}
