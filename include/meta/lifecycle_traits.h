#ifndef DAGGR_LIFECYCLE_TRAITS_H
#define DAGGR_LIFECYCLE_TRAITS_H

/*----- System Includes -----*/

#include <type_traits>

/*----- Type Declarations -----*/

namespace daggr::meta {

  template <class... Ts>
  struct are_default_constructible :
    std::conjunction<
      std::is_default_constructible<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_default_constructible_v = are_default_constructible<Ts...>::value;

  template <class... Ts>
  struct are_nothrow_default_constructible :
    std::conjunction<
      std::is_nothrow_default_constructible<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_nothrow_default_constructible_v = are_nothrow_default_constructible<Ts...>::value;

  template <class... Ts>
  struct are_copy_constructible :
    std::conjunction<
      std::is_copy_constructible<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_copy_constructible_v = are_copy_constructible<Ts...>::value;

  template <class... Ts>
  struct are_nothrow_copy_constructible :
    std::conjunction<
      std::is_copy_constructible<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_nothrow_copy_constructible_v = are_nothrow_copy_constructible<Ts...>::value;

  template <class... Ts>
  struct are_copy_assignable :
    std::conjunction<
      std::is_copy_assignable<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_copy_assignable_v = are_copy_assignable<Ts...>::value;

  template <class... Ts>
  struct are_nothrow_copy_assignable :
    std::conjunction<
      std::is_nothrow_copy_assignable<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_nothrow_copy_assignable_v = are_copy_assignable<Ts...>::value;

  template <class... Ts>
  struct are_move_constructible :
    std::conjunction<
      std::is_move_constructible<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_move_constructible_v = are_move_constructible<Ts...>::value;

  template <class... Ts>
  struct are_nothrow_move_constructible :
    std::conjunction<
      std::is_nothrow_move_constructible<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_nothrow_move_constructible_v = are_nothrow_move_constructible<Ts...>::value;

  template <class... Ts>
  struct are_move_assignable :
    std::conjunction<
      std::is_move_assignable<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_move_assignable_v = are_move_assignable<Ts...>::value;

  template <class... Ts>
  struct are_nothrow_move_assignable :
    std::conjunction<
      std::is_nothrow_move_assignable<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_nothrow_move_assignable_v = are_nothrow_move_assignable<Ts...>::value;

  template <class T>
  struct is_forward_constructible :
    std::disjunction<
      std::conjunction<
        std::is_lvalue_reference<T>,
        std::is_copy_constructible<T>
      >,
      std::conjunction<
        std::negation<std::is_lvalue_reference<T>>,
        std::is_move_constructible<T>
      >
    >
  {};
  template <class T>
  constexpr auto is_forward_constructible_v = is_forward_constructible<T>::value;

  template <class... Ts>
  struct are_forward_constructible :
    std::conjunction<
      is_forward_constructible<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_forward_constructible_v = are_forward_constructible<Ts...>::value;

  template <class T>
  struct is_nothrow_forward_constructible :
    std::disjunction<
      std::conjunction<
        std::is_lvalue_reference<T>,
        std::is_nothrow_copy_constructible<T>
      >,
      std::conjunction<
        std::negation<std::is_lvalue_reference<T>>,
        std::is_nothrow_move_constructible<T>
      >
    >
  {};
  template <class T>
  constexpr auto is_nothrow_forward_constructible_v = is_nothrow_forward_constructible<T>::value;

  template <class... Ts>
  struct are_nothrow_forward_constructible :
    std::conjunction<
      is_nothrow_forward_constructible<Ts>...
    >
  {};
  template <class... Ts>
  constexpr auto are_nothrow_forward_constructible_v = are_nothrow_forward_constructible<Ts...>::value;

  template <class... Ts>
  class copy_guard {

    struct nonesuch {};

    using copyable_t = std::conditional_t<
      (std::is_copy_constructible_v<Ts> && ...),
      copy_guard,
      nonesuch
    >;
    using noncopyable_t = std::conditional_t<
      (!std::is_copy_constructible_v<Ts> && ...),
      copy_guard,
      nonesuch
    >;
    using copy_assignable_t = std::conditional_t<
      (std::is_copy_assignable_v<Ts> && ...),
      copy_guard,
      nonesuch
    >;
    using non_copy_assignable_t = std::conditional_t<
      (!std::is_copy_assignable_v<Ts> && ...),
      copy_guard,
      nonesuch
    >;

    public:

      /*----- Lifecycle Functions -----*/

      copy_guard() = default;
      copy_guard(copyable_t const&) noexcept((std::is_nothrow_copy_constructible_v<Ts> && ...)) {}
      copy_guard(noncopyable_t const&) = delete;
      copy_guard(copy_guard&&) = default;

      /*----- Operators -----*/

      copy_guard& operator =(copy_assignable_t const&) noexcept((std::is_nothrow_copy_assignable_v<Ts> && ...)) {
        return *this;
      }
      copy_guard& operator =(non_copy_assignable_t const&) = delete;
      copy_guard& operator =(copy_guard&&) = default;

      /*----- Public Members -----*/

      static constexpr auto constructible_v = are_copy_constructible_v<Ts...>;
      static constexpr auto assignable_v = are_copy_assignable_v<Ts...>;
      static constexpr auto nothrow_constructible_v = are_nothrow_copy_constructible_v<Ts...>;
      static constexpr auto nothrow_assignable_v = are_nothrow_copy_assignable_v<Ts...>;

  };

  template <class... Ts>
  class move_guard {

    struct nonesuch {};

    using moveable_t = std::conditional_t<
      (std::is_move_constructible_v<Ts> && ...),
      move_guard,
      nonesuch
    >;
    using nonmoveable_t = std::conditional_t<
      (!std::is_move_constructible_v<Ts> && ...),
      move_guard,
      nonesuch
    >;
    using move_assignable_t = std::conditional_t<
      (std::is_move_assignable_v<Ts> && ...),
      move_guard,
      nonesuch
    >;
    using non_move_assignable_t = std::conditional_t<
      (!std::is_move_assignable_v<Ts> && ...),
      move_guard,
      nonesuch
    >;

    public:

      /*----- Lifecycle Functions -----*/

      move_guard() = default;
      move_guard(move_guard const&) = default;
      move_guard(moveable_t&&) noexcept((std::is_nothrow_move_constructible_v<Ts> && ...)) {}
      move_guard(nonmoveable_t&&) = delete;

      /*----- Operators -----*/

      move_guard& operator =(move_guard const&) = default;
      move_guard& operator =(move_assignable_t&&) noexcept((std::is_nothrow_move_assignable_v<Ts> && ...)) {
        return *this;
      }
      move_guard& operator =(non_move_assignable_t&&) = delete;

      /*----- Public Members -----*/

      static constexpr auto constructible_v = are_move_constructible_v<Ts...>;
      static constexpr auto assignable_v = are_move_assignable_v<Ts...>;
      static constexpr auto nothrow_constructible_v = are_nothrow_move_constructible_v<Ts...>;
      static constexpr auto nothrow_assignable_v = are_nothrow_move_assignable_v<Ts...>;

  };

}

#endif
