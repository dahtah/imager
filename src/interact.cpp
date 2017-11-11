#include <imager.h>
using namespace Rcpp;
using namespace cimg_library;

std::string cvt_keycode(const unsigned int key)
{
  switch (key) {
    // case cimg::keySPACE : return "space";
    // case cimg::keyRETURN : return "return";
  case cimg::keyESC : return "esc";
  case cimg::keyF1 : return "f1";
  case cimg::keyF2 : return "f2";
  case cimg::keyF3 : return "f3";
  case cimg::keyF4 : return "f4";
  case cimg::keyF5 : return "f5";
  case cimg::keyF6 : return "f6";
  case cimg::keyF7 : return "f7";
  case cimg::keyF8 : return "f8";
  case cimg::keyF9 : return "f9";
  case cimg::keyF10 : return "f10";
  case cimg::keyF11 : return "f11";
  case cimg::keyF12 : return "f12";
  case cimg::keyPAUSE : return "pause";
  case cimg::key1 : return "1";
  case cimg::key2 : return "2";
  case cimg::key3 : return "3";
  case cimg::key4 : return "4";
  case cimg::key5 : return "5";
  case cimg::key6 : return "6";
  case cimg::key7 : return "7";
  case cimg::key8 : return "8";
  case cimg::key9 : return "9";
  case cimg::key0 : return "0";
  case cimg::keyBACKSPACE : return "backspace";
  case cimg::keyINSERT : return "insert";
  case cimg::keyHOME : return "home";
  case cimg::keyPAGEUP : return "pageup";
  case cimg::keyTAB : return "tab";
  case cimg::keyQ : return "q";
  case cimg::keyW : return "w";
  case cimg::keyE : return "e";
  case cimg::keyR : return "r";
  case cimg::keyT : return "t";
  case cimg::keyY : return "y";
  case cimg::keyU : return "u";
  case cimg::keyI : return "i";
  case cimg::keyO : return "o";
  case cimg::keyP : return "p";
  case cimg::keyDELETE : return "delete";
  case cimg::keyEND : return "end";
  case cimg::keyPAGEDOWN : return "pagedown";
  case cimg::keyCAPSLOCK : return "capslock";
  case cimg::keyA : return "a";
  case cimg::keyS : return "s";
  case cimg::keyD : return "d";
  case cimg::keyF : return "f";
  case cimg::keyG : return "g";
  case cimg::keyH : return "h";
  case cimg::keyJ : return "j";
  case cimg::keyK : return "k";
  case cimg::keyL : return "l";
  case cimg::keyENTER : return "enter";
  case cimg::keyZ : return "z";
  case cimg::keyX : return "x";
  case cimg::keyC : return "c";
  case cimg::keyV : return "v";
  case cimg::keyB : return "b";
  case cimg::keyN : return "n";
  case cimg::keyM : return "m";
#if cimg_OS==2    
  case cimg::keySHIFTLEFT : return "shift";
  case cimg::keyCTRLRIGHT : return "ctrl";
#else
  case cimg::keySHIFTRIGHT : 
  case cimg::keySHIFTLEFT : return "shift";
  case cimg::keyCTRLLEFT : 
  case cimg::keyCTRLRIGHT : return "ctrl";
#endif    
  case cimg::keyARROWUP : return "arrowup";
  case cimg::keyALT : return "alt";
  case cimg::keySPACE : return "space";
    //  case cimg::keyALTGR : return "altgr";
  case cimg::keyMENU : return "menu";
  case cimg::keyARROWLEFT : return "arrowleft";
  case cimg::keyARROWDOWN : return "arrowdown";
  case cimg::keyARROWRIGHT : return "arrowright";
  case cimg::keyPAD0 : return "pad0";
  case cimg::keyPAD1 : return "pad1";
  case cimg::keyPAD2 : return "pad2";
  case cimg::keyPAD3 : return "pad3";
  case cimg::keyPAD4 : return "pad4";
  case cimg::keyPAD5 : return "pad5";
  case cimg::keyPAD6 : return "pad6";
  case cimg::keyPAD7 : return "pad7";
  case cimg::keyPAD8 : return "pad8";
  case cimg::keyPAD9 : return "pad9";
  case cimg::keyPADADD : return "padadd";
  case cimg::keyPADSUB : return "padsub";
  case cimg::keyPADMUL : return "padmul";
  case cimg::keyPADDIV : return "paddiv";	
  }
  return "unknown";
  
}


// [[Rcpp::export]]
NumericVector interact_(Function fun,NumericVector init,std::string title = "")
{
  List state;
  CId img=as<CId >(init);
  state["mouse_x"] = -1;
  state["mouse_y"] = -1;
  state["mouse_button"] = 0;
  state["mouse_wheel"] = 0;
  state["key"] = "";
  //NumericVector im = fun(state);
  //CId img = as<CId >(im);
  //  CImgDisplay disp(255*img,title.c_str(),0,false,false);
  CImgDisplay disp(img.width(),img.height(),title.c_str(),0,false,false);
  disp.display(255*img);

  //int a =0;
    while (true)
  //  while (false)
    {
      if (disp.is_closed() or disp.is_key(cimg::keyESC))
	{
	  break;
	}
      state["mouse_x"] = disp.mouse_x() + 1;
      state["mouse_y"] = disp.mouse_y() + 1;
      state["mouse_button"] = disp.button();
      state["mouse_wheel"] = disp.wheel();
      
      state["key"] = (disp.key() ? cvt_keycode(disp.key()) : "");
      {
	Rcpp::Nullable< NumericVector> inp =  fun(state);
	if (inp.isNotNull())
	  {
	    NumericVector nim(inp);
	    CId nimg = as<CId >(nim);
	    if (disp.width() != nimg.width() or disp.height() != nimg.height())
	      {
		disp.resize(nimg.width(),nimg.height());
	      }
	    disp.display(255*nimg);
	  }
      }

      disp.wait();
      Rcpp::checkUserInterrupt();
    }
    CId out(disp);
    return wrap(out);
}
