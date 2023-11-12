module kernel

	real, parameter :: INF=1.0e8
	type vec3
	real :: x,y,z
	
	contains
		procedure :: mg => mg_vec
		procedure :: rscal => rscal_vec
		procedure :: normal => normal_vec
	end type vec3

	public :: operator(+)
	public :: operator(-)
	
	interface operator(+)
		procedure plus_vec
	end interface
	
	interface operator(-)
		procedure minus_vec
	end interface
	
	type, extends(vec3) :: sphere
	
	real :: r
	type(vec3) :: color
	
	contains
		procedure :: get_origin => g_origin
		procedure :: get_color => g_color
	end type
	
	contains

	
	function g_origin(this) result(w)
	class(sphere), intent(in) :: this
	type(vec3) :: w
	
	w%x=this%x
	w%y=this%y
	w%z=this%z
	
	end function
	
	function g_color(this) result(w)
	class(sphere), intent(in) :: this
	type(vec3) :: w
	
	w%x=this%color%x
	w%y=this%color%y
	w%z=this%color%z
	
	end function
	
	function normal_vec(this) result(w)
	class(vec3), intent(in) :: this
	type(vec3) :: w
	real :: mg
	
	mg=sqrt(this%x*this%x+this%y*this%y+this%z*this%z)
	
	w%x=this%x/mg
	w%y=this%y/mg
	w%z=this%z/mg
	
	end function
	
	real function mg_vec(this)
		class(vec3), intent(in) :: this
		
		mg_vec=sqrt(this%x*this%x + this%y*this%y + this%z*this%z )
		
	end function
	
	function rscal_vec(this,a) result(w)
	class(vec3), intent(in) :: this
	type(vec3) :: w
	
	w%x=this%x*a
	w%y=this%y*a
	w%z=this%z*a
	
	end function
	
	
	function plus_vec(u,v) result(w)
	type(vec3), intent(in) :: u,v
	type(vec3) :: w
	
	w%x=u%x+v%x
	w%y=u%y+v%y
	w%z=u%z+v%z
	
	end function
	
	function minus_vec(u,v) result(w)
	type(vec3), intent(in) :: u,v
	type(vec3) :: w
	
	w%x=u%x-v%x
	w%y=u%y-v%y
	w%z=u%z-v%z
	
	end function
	
	function dot(a,b) result(w)
	type(vec3), intent(in) :: a,b
	real :: w
	w=a%x*b%x + a%y*b%y + a%z*b%z
	end function
	
	function ray_hit(o,d,t) result (w)
	type(vec3), intent(in) :: o,d
	type(sphere), intent(in) :: t
	type(vec3) :: oc
	real :: disc,a,b,c
	logical :: w
	
	
	oc=o-t.get_origin()
	
	
	a=dot(d,d)
	b=2.0*dot(oc,d)
	c=dot(oc,oc)-t%r*t%r
	disc=b*b-4.0*a*c

	if(disc < 1.0e-4) then
		w=.false.
	else
		w=.true.
	end if
	
	end function
	
	function ray_intsc(o,d,t) result (w)
	type(vec3), intent(in) :: o,d
	type(sphere), intent(in) :: t
	type(vec3) :: oc
	real :: disc,a,b,c,t1,t2
	real :: w

	
	oc=o-t.get_origin()
	
	a=dot(d,d)
	b=2.0*dot(oc,d)
	c=dot(oc,oc)-t%r*t%r
	disc=b*b-4.0*a*c

	t1=(-b+sqrt(disc))/(2.0*a)
	t2=(-b-sqrt(disc))/(2.0*a)
	
	if(disc < 1.0e-4 ) then
		w=INF
	else
		w=t2
	end if
	
	end function
		
	subroutine clamp255(pixel)
		type(vec3) :: pixel
		
		if(pixel%x > 255) pixel%x=255
		if(pixel%x < 0) pixel%x=0
		
		if(pixel%y > 255) pixel%y=255
		if(pixel%y < 0) pixel%y=0
		
		if(pixel%z > 255) pixel%z=255
		if(pixel%z < 0) pixel%z=0
		
	end subroutine	
end module
