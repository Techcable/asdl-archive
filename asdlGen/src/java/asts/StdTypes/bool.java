/* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_import asdl_base
  --line_width 74
  --no_action false
  --output_directory ./
  --view Java
  */
package asts.StdTypes;
import asdl_base.*;
public final class bool {
    public final static int TRUE_enum = 0;
    public final static int FALSE_enum = 1;
    
    private int kind_;
    
    public bool(int kind)
    {
        this.kind_ = kind;
    }
   
    public final int kind()
     {
         return kind_;
     }
    public final void accept(Visitor v)
     {
         switch(this.kind()) {
             case bool.TRUE_enum:
             v.visit_TRUE(this);
             break;
             case bool.FALSE_enum:
             v.visit_FALSE(this);
             break;
             default: 
             g.die();
             
         }
     }
    public final static boolean read(java.io.InputStream s_)
     {
         boolean ret_;
         ret_ = g.read_java_boolean(s_);
         return ret_;
     }
    public final static void write(boolean x_, java.io.OutputStream s_)
     {
         g.write_java_boolean(x_, s_);
     }
    public final static boolean read_tagged(java.io.InputStream s_)
     {
         boolean ret_;
         if(g.read_tag(s_) != 5) g.die();
         ret_ = bool.read(s_);
         return ret_;
     }
    public final static void write_tagged(boolean x_,
                                          java.io.OutputStream s_)
     {
         g.write_tag(5, s_);
         bool.write(x_, s_);
     }
}
