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
public final class uint64 {
    public int int1;
    
    public uint64(int int1)
    {
        this.int1 = int1;
    }
    public final static void write_option(java.math.BigInteger x_,
                                          java.io.OutputStream s_)
     {
         if(x_ != null)
             {
                 g.write_tag(1, s_);
                 uint64.write(x_, s_);
             }
          else
             g.write_tag(0, s_);
     }
    public final static java.math.BigInteger read_option(java.io.InputStream s_)
     {
         java.math.BigInteger ret_;
         if(g.read_tag(s_) != 0) ret_ = uint64.read(s_); else ret_ = null;
         return ret_;
     }
    public final void accept(Visitor v)
     {
         v.visit_uint64(this);
     }
    public final static java.math.BigInteger read(java.io.InputStream s_)
     {
         java.math.BigInteger ret_;
         ret_ = g.read_java_BigInteger(s_);
         return ret_;
     }
    public final static void write(java.math.BigInteger x_,
                                   java.io.OutputStream s_)
     {
         g.write_java_BigInteger(x_, s_);
     }
    public final static java.math.BigInteger read_tagged(java.io.InputStream s_)
     {
         java.math.BigInteger ret_;
         if(g.read_tag(s_) != 13) g.die();
         ret_ = uint64.read(s_);
         return ret_;
     }
    public final static void write_tagged(java.math.BigInteger x_,
                                          java.io.OutputStream s_)
     {
         g.write_tag(13, s_);
         uint64.write(x_, s_);
     }
}