import React from 'react';

const InternalError = () => {
  return (
    <main className="grid min-h-full place-items-center bg-white px-6 py-24 sm:py-32 lg:px-8">
      <div className="text-center">
        <p className="text-base font-semibold text-indigo-600">500</p>
        <h1 className="mt-4 text-3xl font-bold tracking-tight text-gray-900 sm:text-5xl">
          Internal Server Error
        </h1>
        <p className="mt-6 text-base leading-7 text-gray-600">
          Oops! Something went wrong on our end. Please try again later.
        </p>
        <div className="mt-10 flex items-center justify-center gap-x-6">
          <a href="/" className="text-sm font-semibold text-gray-900">
            Go back home
          </a>
          <a
            href="https://blockapps.net/company/connect/contact-us/"
            className="text-sm font-semibold text-gray-900"
          >
            Contact support <span aria-hidden="true">&rarr;</span>
          </a>
        </div>
      </div>
    </main>
  );
};

export default InternalError;
